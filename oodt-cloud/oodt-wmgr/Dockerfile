# Docker image for OODT Workflow Manager

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# OODT
ENV OODT_HOME /usr/local/oodt
ENV OODT_VERSION 1.0
RUN mkdir -p $OODT_HOME
COPY pom.xml /tmp/pom.xml
RUN cd /tmp && mvn install

# create symbolic links
RUN cd $OODT_HOME && \
    ln -s ./cas-workflow-${OODT_VERSION} ./cas-workflow

# install scripts
COPY start.sh /usr/local/bin/start-wmgr.sh
COPY stop.sh /usr/local/bin/stop-wmgr.sh

# create directories that don't exist
RUN mkdir ${OODT_HOME}/run

# customize workflow.properties
COPY workflow.properties $OODT_HOME/cas-workflow/etc/workflow.properties
RUN mkdir -p $OODT_HOME/config/workflows

# FIXME
RUN mkdir -p $OODT_HOME/config/workflows/test-workflow
COPY policy/ $OODT_HOME/config/workflows/test-workflow/policy/
COPY pge-configs/ $OODT_HOME/config/workflows/test-workflow/pge-configs/

ENV PGE_ROOT /usr/local/oodt-pges
RUN mkdir -p $PGE_ROOT

ENV OODT_ARCHIVE_DIR /usr/local/oodt-archive
run mkdir -p $OODT_ARCHIVE_DIR

ENV WORKFLOW_URL http://localhost:9001/
ENV FILEMGR_URL http://filemgr.oodt.net:9000/

EXPOSE 9001

# FIXME
ENTRYPOINT ["/usr/local/bin/start-wmgr.sh"]
#ENTRYPOINT ["/bin/bash"]
