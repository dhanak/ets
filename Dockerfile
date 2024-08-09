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
ARG MOD_AUTH_OPENIDC_VERSION=2.4.12.3-2+deb12u1
RUN wget -P /tmp \
    http://ftp.hu.debian.org/debian/pool/main/liba/libapache2-mod-auth-openidc/libapache2-mod-auth-openidc_${MOD_AUTH_OPENIDC_VERSION}_amd64.deb && \
    dpkg-deb -x /tmp/libapache2-mod-auth-openidc_${MOD_AUTH_OPENIDC_VERSION}_amd64.deb /tmp && \
    cp /tmp/usr/lib/apache2/modules/mod_auth_openidc.so ${HTTPD_PREFIX}/modules

# install perl dependencies
RUN mkdir -p /var/cache/perl/.cpan /var/cache/perl/.cpanm && \
    ln -s /var/cache/perl/.cpan* /root && \
    cpan App::cpanminus && cpanm \
        Apache::DBI \
        CGI \
        Data::Dumper \
        DBD::MariaDB \
        Email::Stuffer \
        File::MMagic \
        HTML::Mason \
        URI \
        UUID && \
    rm /root/.cpan*

# set environment variables
ENV ETS_ROOT=/opt/ets

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

# create mason working directory
RUN mkdir mason && chown www-data:www-data mason

# switch back to Apache 1.3 compatible mpm_prefork (for perl)
# append the config to the end of the global apache config
RUN sed -i \
        -e 's/LoadModule mpm_event/#LoadModule mpm_event/' \
        -e 's/#LoadModule mpm_prefork/LoadModule mpm_prefork/' \
        ${HTTPD_PREFIX}/conf/httpd.conf && \
    cat apache/openidc.conf apache/ets.conf >>${HTTPD_PREFIX}/conf/httpd.conf
