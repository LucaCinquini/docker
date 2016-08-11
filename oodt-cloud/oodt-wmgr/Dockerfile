# Docker image for OODT Workflow Manager

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# OODT env
ENV OODT_VERSION 1.0
ENV OODT_HOME /usr/local/oodt
ENV WORKFLOW_URL http://localhost:9001/
ENV FILEMGR_URL http://filemgr.oodt.net:9000/
ENV OODT_SITE_DIR ${OODT_SITE_DIR:-${OODT_HOME}/mysite}
ENV PGE_ROOT $OODT_SITE_DIR/pges

# install OODT Workflow Manager
RUN mkdir -p $OODT_HOME
COPY pom.xml /tmp/pom.xml
RUN cd /tmp && mvn install

# create symbolic links
RUN cd $OODT_HOME && \
    ln -s ./cas-workflow-${OODT_VERSION} ./cas-workflow

# create directories that don't exist
RUN mkdir ${OODT_HOME}/run
RUN mkdir -p $PGE_ROOT
# customize workflow.properties
COPY workflow.properties $OODT_HOME/cas-workflow/etc/workflow.properties
RUN mkdir -p $OODT_SITE_DIR/workflows

# install scripts
COPY start.sh /usr/local/bin/start-wmgr.sh
COPY stop.sh /usr/local/bin/stop-wmgr.sh
COPY supervisord.conf /etc/supervisor/supervisord-workflow.conf

EXPOSE 9001

# FIXME
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord-workflow.conf"]
#ENTRYPOINT ["/usr/local/bin/start-wmgr.sh"]
