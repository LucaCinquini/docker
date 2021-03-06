# CoG image configured for ESGF node
FROM esgfhub/cog:v3.6.1

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# install and configure postgres
RUN yum -y update; yum clean all
RUN yum -y install epel-release; yum clean all
RUN yum -y install postgresql-server postgresql postgresql-contrib supervisor; yum clean all

ADD ./postgresql-setup /usr/bin/postgresql-setup
ADD ./postgres_esgf.sh /postgres_esgf.sh
ADD ./supervisord.conf /etc/supervisord.conf
ADD ./start_postgres.sh /start_postgres.sh
ADD ./esgf_security.sql /tmp/esgf_security.sql

RUN chmod +x /usr/bin/postgresql-setup
RUN chmod +x /start_postgres.sh
RUN chmod +x /postgres_esgf.sh

RUN /usr/bin/postgresql-setup initdb

ADD ./postgresql.conf /var/lib/pgsql/data/postgresql.conf

RUN chown -v postgres.postgres /var/lib/pgsql/data/postgresql.conf

RUN echo "host    all             all             0.0.0.0/0               md5" >> /var/lib/pgsql/data/pg_hba.conf

RUN /postgres_esgf.sh

EXPOSE 5432

# IMPORTANT: change connections to require encrypted password
RUN sed -i 's/ident/md5/g' /var/lib/pgsql/data/pg_hba.conf

# scripted commands use password in ~/.pgpass
RUN echo 'localhost:5432:cogdb:dbsuper:changeit' >> /root/.pgpass
RUN chmod 0600 /root/.pgpass

# write credentials to access postgres database
# in locations where ESGF/CoG can find them
# credentials must match those configured in /postgres_esgf.sh
RUN mkdir -p /esg/config
RUN echo 'changeit' >> /esg/config/.esg_pg_pass
RUN echo 'db.user=dbsuper' >> /esg/config/esgf.properties
RUN echo 'db.host=localhost' >> /esg/config/esgf.properties
RUN echo 'db.port=5432' >> /esg/config/esgf.properties
RUN echo 'changeit' >> /esg/config/.esgf_pass

# default working directory
WORKDIR $COG_INSTALL_DIR

# start up django+CoG with correct host IP
COPY ./docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]

# default host IP
CMD ["localhost"]
