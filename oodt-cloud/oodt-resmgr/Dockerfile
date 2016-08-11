# Docker image for OODT Resource Manager

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# OODT env
ENV OODT_VERSION 1.0
ENV OODT_HOME /usr/local/oodt

# install OODT Resource Manager
RUN mkdir -p $OODT_HOME
COPY pom.xml /tmp/pom.xml
RUN cd /tmp && mvn install

# create directories that don't exist
RUN mkdir ${OODT_HOME}/run

# create symbolic links
RUN cd $OODT_HOME && \
    ln -s ./cas-resource-${OODT_VERSION} ./cas-resource

# install scripts
COPY start.sh /usr/local/bin/start_resmgr.sh
COPY stop.sh /usr/local/bin/stop_resmgr.sh
COPY supervisord.conf /etc/supervisor/supervisord-resmgr.conf

EXPOSE 9002

#ENTRYPOINT ["/usr/local/bin/start_resmgr.sh"]
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord-resmgr.conf"]
