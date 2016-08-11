# Docker image for OODT File Manager configured with Solr back-end

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# OODT env
ENV OODT_VERSION 1.0
ENV OODT_HOME /usr/local/oodt
ENV OODT_ARCHIVE_DIR ${OODT_ARCHIVE_DIR:-${OODT_HOME}/archive}
ENV OODT_SITE_DIR ${OODT_SITE_DIR:-${OODT_HOME}/mysite}

# Solr env
ENV SOLR_VERSION 5.3.1
ENV SOLR_INSTALL_DIR ${OODT_HOME}/solr
ENV SOLR_HOME ${OODT_HOME}/solr-home
ENV SOLR_DATA_DIR ${SOLR_DATA_DIR:-${OODT_HOME}/solr-index}

# install OODT File Manager
RUN mkdir -p $OODT_HOME
COPY pom.xml /tmp/pom.xml
RUN cd /tmp && mvn install

# create symbolic links
RUN cd $OODT_HOME && \
    ln -s ./cas-filemgr-${OODT_VERSION} ./cas-filemgr

# create directories that don't exist
RUN mkdir -p ${OODT_HOME}/run
RUN mkdir -p ${OODT_HOME}/cas-filemgr/logs

# final location or published products
RUN mkdir -p $OODT_ARCHIVE_DIR

# location of custom policy files
RUN mkdir -p $OODT_SITE_DIR/products

# customize filemgr.properties
COPY filemgr.properties $OODT_HOME/cas-filemgr/etc/filemgr.properties

# expose port to other containers
EXPOSE 9000

# Solr
# FIXME
#RUN wget -O /tmp/solr-${SOLR_VERSION}.tgz http://archive.apache.org/dist/lucene/solr/${SOLR_VERSION}/solr-${SOLR_VERSION}.tgz
COPY solr-${SOLR_VERSION}.tgz /tmp/solr-${SOLR_VERSION}.tgz
RUN cd ${OODT_HOME} && \
    tar xzf /tmp/solr-${SOLR_VERSION}.tgz && \
    rm /tmp/solr-${SOLR_VERSION}.tgz && \
    ln -s ./solr-${SOLR_VERSION} ./solr
RUN mkdir -p $SOLR_DATA_DIR
# copy solr-home directory tree (recursively)
COPY solr-home/ $SOLR_HOME/
# expose port to other containers
EXPOSE 8983

# install start/stop scripts
COPY start.sh /usr/local/bin/start-filemgr.sh
COPY stop.sh /usr/local/bin/stop-filemgr.sh
COPY supervisord.conf /etc/supervisor/supervisord-filemgr.conf

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord-filemgr.conf"]
#ENTRYPOINT ["/usr/local/bin/start-filemgr.sh"]
