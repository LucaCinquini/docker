# Docker image for OODT Catalog and Archive Services
# (core services only)

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
    ln -s ./cas-crawler-${OODT_VERSION} cas-crawler && \
    ln -s ./cas-filemgr-${OODT_VERSION} ./cas-filemgr && \
    ln -s ./cas-resource-${OODT_VERSION} ./cas-resource && \
    ln -s ./cas-workflow-${OODT_VERSION} ./cas-workflow

# install scripts
COPY start.sh ${OODT_HOME}/.
COPY stop.sh ${OODT_HOME}/.

# create directories that don't exist
RUN mkdir ${OODT_HOME}/run
RUN mkdir ${OODT_HOME}/cas-filemgr/logs

EXPOSE 9000
EXPOSE 9001
EXPOSE 9002

ENV FILEMGR_URL http://localhost:9000
ENV WORKFLOW_URL http://localhost:9001

ENTRYPOINT ["/usr/local/oodt/start.sh"]
