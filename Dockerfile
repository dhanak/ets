ARG APACHE_VERSION=2.4.61
ARG DEBIAN_VERSION=bookworm
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
        IO::Socket::SSL \
        HTML::Mason \
        Math::Round \
        Text::CSV \
        URI \
        UUID

# set environment variables
ENV ETS_ROOT=/opt/ets
ENV DB_ARCHIVE_DIR=/mnt/archives

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

# set up volume
RUN mkdir ${DB_ARCHIVE_DIR} && chmod a+rwx,+t ${DB_ARCHIVE_DIR}
VOLUME ${DB_ARCHIVE_DIR}

##
## GUTS install image
##
FROM perl:${PERL_VERSION}-${DEBIAN_VERSION} AS guts-build

ARG ERLANG_VERSION=27.0.1
ARG ELIXIR_VERSION=1.17.2
ARG SICSTUS_VERSION=4.9.0
ARG SICSTUS_PLATFORM=x86_64-linux-glibc2.28

# download and install kerl, install erlang
RUN wget https://raw.githubusercontent.com/kerl/kerl/master/kerl && \
    chmod a+x kerl && \
    KERL_DEBUG=1 ./kerl build-install ${ERLANG_VERSION} ${ERLANG_VERSION} /opt/erlang

# download and extract elixir
RUN wget -O/dev/shm/elixir.zip \
    "https://builds.hex.pm/builds/elixir/v${ELIXIR_VERSION}.zip" && \
    unzip -d/opt/elixir /dev/shm/elixir.zip

# download and extract prolog from SICStus website
RUN wget -O- https://sicstus.sics.se/sicstus/products4/sicstus/${SICSTUS_VERSION}/binaries/linux/sp-${SICSTUS_VERSION}-${SICSTUS_PLATFORM}.tar.gz | tar xz

WORKDIR /usr/src/app/sp-${SICSTUS_VERSION}-${SICSTUS_PLATFORM}

ARG SICSTUS_SITENAME
ARG SICSTUS_LICENSE_CODE
ARG SICSTUS_EXPIRATION_DATE

# put install.cache in place
COPY <<EOF install.cache
    installdir='/opt/sicstus'
    sitename='${SICSTUS_SITENAME}'
    licensecode='${SICSTUS_LICENSE_CODE}'
    expires='${SICSTUS_EXPIRATION_DATE}'
EOF

# run batch installation
RUN ./InstallSICStus --batch

# extend path
ENV PATH=/opt/sicstus/bin:${PATH}
ENV LD_LIBRARY_PATH=/opt/sicstus/lib

WORKDIR /opt/guts

# copy and build guts binaries
COPY ./guts/ ./
RUN make && make clean

##
## guts runtime image
##
FROM debian:${DEBIAN_VERSION}-slim AS guts-runtime

# install extra debian dependencies
RUN --mount=type=cache,id=apt-global,sharing=locked,target=/var/cache/apt \
    apt-get update && \
    apt-get install -y busybox curl file locales make procps \
        liblockfile-bin libmariadb3 && \
    busybox --install

# configure UTF-8 locale (for SICStus)
ENV LANG=en_US.UTF-8
RUN sed -i -e "s/# $LANG.*/$LANG UTF-8/" /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=$LANG

# copy from build image
COPY --from=guts-build /opt/erlang/  /opt/erlang/
COPY --from=guts-build /opt/elixir/  /opt/elixir/
COPY --from=guts-build /opt/sicstus/ /opt/sicstus/
COPY --from=guts-build /opt/guts/    /opt/guts/

ENV GUTS_ROOT=/opt/guts
ENV GUTS_WORK_DIR=${GUTS_ROOT}/work

# add executable permissions
RUN chmod -R a+rX ${GUTS_ROOT}

# extend path
ENV PATH=/opt/sicstus/bin:/opt/elixir/bin:/opt/erlang/bin:${PATH}
ENV LD_LIBRARY_PATH=/opt/sicstus/lib

# setup workdir volume
RUN mkdir ${GUTS_WORK_DIR} && \
    for dir in daemons env hwks spools; do \
       mkdir ${GUTS_WORK_DIR}/${dir} && chown nobody ${GUTS_WORK_DIR}/${dir}; \
    done
VOLUME ${GUTS_WORK_DIR}

# change user of the guts process
USER nobody

# workdir and netcat server command
WORKDIR ${GUTS_ROOT}
CMD ["nc", "-p", "31337", "-ll", "-e", "./guts-serve.sh"]
