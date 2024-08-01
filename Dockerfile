ARG APACHE_VERSION=2.4.61
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
## ETS image
##
FROM motemen/mod_perl:${PERL_VERSION}-${APACHE_VERSION} AS ets

# install extra debian dependencies
RUN --mount=type=cache,id=apt-global,sharing=locked,target=/var/cache/apt \
    apt-get update && \
    apt-get install -y curl gcc gettext-base libmariadb-dev make

# install perl dependencies
RUN --mount=type=cache,id=perl-cache,sharing=locked,target=/var/cache/perl \
    mkdir -p /var/cache/perl/.cpan /var/cache/perl/.cpanm && \
    ln -s /var/cache/perl/.cpan* /root && \
    cpan App::cpanminus && cpanm \
        Apache::DBI \
        CGI \
        Data::Dumper \
        DBD::MariaDB \
        File::MMagic \
        HTML::Mason \
        Mail::Mailer \
        URI \
        UUID && \
    rm /root/.cpan*

# set environment variables
ENV ETS_ROOT=/opt/ets

# set working directory
WORKDIR ${ETS_ROOT}

# copy contents
COPY --chown=www-data apache      ./apache
COPY --chown=www-data comps       ./comps
COPY --chown=www-data modules     ./modules
COPY --chown=www-data public_html ./public_html

# create mason working directory
RUN mkdir mason && chown www-data:www-data mason

# substitute envvars in template files
RUN for f in */*.template; do \
        envsubst '$ETS_ROOT' <${f} >${f%.template}; \
        chown www-data:www-data ${f%.template}; \
    done

# switch back to Apache 1.3 compatible mpm_prefork (for perl)
# append the config to the end of the global apache config
RUN sed -i \
        -e 's/LoadModule mpm_event/#LoadModule mpm_event/' \
        -e 's/#LoadModule mpm_prefork/LoadModule mpm_prefork/' \
        ${HTTPD_PREFIX}/conf/httpd.conf && \
    cat apache/ets.conf >>${HTTPD_PREFIX}/conf/httpd.conf
