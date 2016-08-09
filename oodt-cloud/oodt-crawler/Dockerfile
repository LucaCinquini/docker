# Docker image for OODT Crawler

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
    ln -s ./cas-crawler-${OODT_VERSION} cas-crawler

# keep container running
ENTRYPOINT ["tail", "-f", "/dev/null"]
