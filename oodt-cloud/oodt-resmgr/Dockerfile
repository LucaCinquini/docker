# Docker image for OODT Resource Manager

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# OODT
ENV OODT_HOME /usr/local/oodt
ENV OODT_VERSION 0.10
RUN mkdir -p $OODT_HOME
COPY pom.xml /tmp/pom.xml
RUN cd /tmp && mvn install

# create symbolic links
RUN cd $OODT_HOME && \
    ln -s ./cas-resource-${OODT_VERSION} ./cas-resource

# install scripts
COPY start.sh /usr/local/bin/start_resmgr.sh
COPY stop.sh /usr/local/bin/stop_resmgr.sh

# create directories that don't exist
RUN mkdir ${OODT_HOME}/run

EXPOSE 9002

ENTRYPOINT ["/usr/local/bin/start_resmgr.sh"]
