ARG ALPINE_VERSION=3.20
ARG APACHE_VERSION=2.4.61
ARG KEYCLOAK_VERSION=25.0.2
ARG PERL_VERSION=5.40.0
ARG MARIADB_VERSION=lts

##
## MariaDB image
##
FROM mariadb:${MARIADB_VERSION} AS db

# copy configuration file
COPY --chown=mysql sql/scheduler.cnf /etc/mysql/conf.d/

# initialize tables
COPY --chown=mysql \
     sql/create-tables-sql.sh sql/labs.sql sql/regular-cleanup.sql \
     /docker-entrypoint-initdb.d/

##
## Keycloak server
##
FROM quay.io/keycloak/keycloak:${KEYCLOAK_VERSION} AS keycloak

# copy initial realm data
COPY ets-realm.json /opt/keycloak/data/import/

##
## SICStus install image
##
FROM alpine:${ALPINE_VERSION} AS sicstus

ARG SICSTUS_VERSION=4.9.0
ARG SICSTUS_PLATFORM=x86_64-linux-glibc2.28

RUN wget -O- https://sicstus.sics.se/sicstus/products4/sicstus/${SICSTUS_VERSION}/binaries/linux/sp-${SICSTUS_VERSION}-${SICSTUS_PLATFORM}.tar.gz | tar xz

WORKDIR sp-${SICSTUS_VERSION}-${SICSTUS_PLATFORM}

ARG SICSTUS_SITENAME
ARG SICSTUS_LICENSE_CODE
ARG SICSTUS_EXPIRATION_DATE

COPY <<EOF install.cache
    installdir='/opt/sicstus'
    sitename='${SICSTUS_SITENAME}'
    licensecode='${SICSTUS_LICENSE_CODE}'
    expires='${SICSTUS_EXPIRATION_DATE}'
EOF

RUN ./InstallSICStus --batch

##
## ETS image
##
FROM motemen/mod_perl:${PERL_VERSION}-${APACHE_VERSION} AS ets

# install extra debian dependencies
RUN --mount=type=cache,id=apt-global,sharing=locked,target=/var/cache/apt \
    apt-get update && \
    apt-get install -y busybox gcc libmariadb-dev make wget \
        # libapache2-mod-auth-openidc dependencies
        libcjose0 libhiredis0.14 && \
    busybox --install

# install libapache2-mod-auth-openidc
ARG MOD_AUTH_OPENIDC_VERSION=2.4.15.7-1.bookworm
RUN wget -P /tmp \
    https://github.com/OpenIDC/mod_auth_openidc/releases/download/v${MOD_AUTH_OPENIDC_VERSION%-*}/libapache2-mod-auth-openidc_${MOD_AUTH_OPENIDC_VERSION}_amd64.deb && \
    dpkg-deb -x /tmp/libapache2-mod-auth-openidc_${MOD_AUTH_OPENIDC_VERSION}_amd64.deb /tmp && \
    cp /tmp/usr/lib/apache2/modules/mod_auth_openidc.so ${HTTPD_PREFIX}/modules

# install perl dependencies
RUN cpan App::cpanminus && cpanm \
        Apache::DBI \
        CGI \
        Data::Dumper \
        DBD::MariaDB \
        Email::Stuffer \
        File::MMagic \
        HTML::Mason \
        URI \
        UUID

# set environment variables
ENV ETS_ROOT=/opt/ets
ENV DB_ARCHIVE_DIR=/mnt/archives
ENV GUTS_WORK_DIR=/mnt/guts

# set working directory
WORKDIR ${ETS_ROOT}

# download TinyMCE community language pack
RUN mkdir -p public_html/include && \
    wget -O- https://download.tiny.cloud/tinymce/community/languagepacks/6/hu_HU.zip | \
    unzip - -d public_html/include/

# copy contents
COPY --chown=www-data apache      ./apache
COPY --chown=www-data comps       ./comps
COPY --chown=www-data modules     ./modules
COPY --chown=www-data public_html ./public_html

# create mason working directory, fix temp dir permissions
RUN mkdir mason && chown www-data:www-data mason && chmod a+rwx,+t /tmp

# switch back to Apache 1.3 compatible mpm_prefork (for perl)
# append the config to the end of the global apache config
RUN sed -i \
        -e 's/LoadModule mpm_event/#LoadModule mpm_event/' \
        -e 's/#LoadModule mpm_prefork/LoadModule mpm_prefork/' \
        ${HTTPD_PREFIX}/conf/httpd.conf && \
    cat apache/openidc.conf apache/ets.conf >>${HTTPD_PREFIX}/conf/httpd.conf

# set up volumes
RUN mkdir ${DB_ARCHIVE_DIR} && chmod a+rwx,+t ${DB_ARCHIVE_DIR}
RUN mkdir ${GUTS_WORK_DIR} && for dir in env hwks logs mails spools; do \
        mkdir ${GUTS_WORK_DIR}/${dir}; \
    done
VOLUME ${DB_ARCHIVE_DIR}
VOLUME ${GUTS_WORK_DIR}
