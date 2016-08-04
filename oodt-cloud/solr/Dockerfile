# Docker image for Solr engine configured with default OODT metadata schema

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

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

# copy start/stop scripts
COPY *.sh $SOLR_INSTALL_DIR/bin/

EXPOSE 8983

ENTRYPOINT ["/usr/local/solr/bin/start.sh"]
