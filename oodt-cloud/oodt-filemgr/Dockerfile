# Docker image for OODT File Manager with Solr back-end

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# OODT File Manager
ENV OODT_HOME /usr/local/oodt
ENV OODT_VERSION 0.10
RUN mkdir -p $OODT_HOME
COPY pom.xml /tmp/pom.xml
RUN cd /tmp && mvn install
# create symbolic links
RUN cd $OODT_HOME && \
    ln -s ./cas-filemgr-${OODT_VERSION} ./cas-filemgr
# create directories that don't exist
RUN mkdir -p ${OODT_HOME}/run
RUN mkdir -p ${OODT_HOME}/cas-filemgr/logs
# customize filemgr.properties
COPY filemgr.properties $OODT_HOME/cas-filemgr/etc/filemgr.properties
# expose port to other containers
EXPOSE 9000

# Solr
ENV SOLR_VERSION 5.3.1
ENV SOLR_INSTALL_DIR /usr/local/solr
ENV SOLR_HOME /usr/local/solr-home
ENV SOLR_DATA_DIR /usr/local/solr-index
# FIXME
#RUN wget -O /tmp/solr-${SOLR_VERSION}.tgz http://archive.apache.org/dist/lucene/solr/${SOLR_VERSION}/solr-${SOLR_VERSION}.tgz
COPY solr-${SOLR_VERSION}.tgz /tmp/solr-${SOLR_VERSION}.tgz
RUN cd /usr/local && \
    tar xzf /tmp/solr-${SOLR_VERSION}.tgz && \
    rm /tmp/solr-${SOLR_VERSION}.tgz && \
    ln -s ./solr-${SOLR_VERSION} ./solr
RUN mkdir -p $SOLR_DATA_DIR
# copy solr-home directory tree (recursively)
COPY solr-home/ $SOLR_HOME/
# expose port to other containers
EXPOSE 8983

# install scripts
COPY start.sh /usr/local/bin/start-filemgr.sh
COPY stop.sh /usr/local/bin/stop-filemgr.sh

ENTRYPOINT ["/usr/local/bin/start-filemgr.sh"]
