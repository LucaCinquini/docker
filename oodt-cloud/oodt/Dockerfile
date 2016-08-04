# Docker image for OODT installation server

FROM centos:centos6.7

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# Update to last version
RUN yum -y update; yum clean all

# install wget
RUN yum -y update && \
    yum -y install wget && \
    yum clean all

# Java 8
ENV JAVA_VERSION 8u31
ENV BUILD_VERSION b13
RUN wget --no-cookies --no-check-certificate --header "Cookie: oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/$JAVA_VERSION-$BUILD_VERSION/jdk-$JAVA_VERSION-linux-x64.rpm" -O /tmp/jdk-8-linux-x64.rpm

RUN yum -y install /tmp/jdk-8-linux-x64.rpm && \
    rm /tmp/jdk-8-linux-x64.rpm

RUN alternatives --install /usr/bin/java jar /usr/java/latest/bin/java 200000
RUN alternatives --install /usr/bin/javaws javaws /usr/java/latest/bin/javaws 200000
RUN alternatives --install /usr/bin/javac javac /usr/java/latest/bin/javac 200000

ENV JAVA_HOME /usr/java/latest

# Git
RUN yum -y update && \
    yum -y install git && \
    yum clean all && \
    git --version

# Maven

ENV MAVEN_VERSION 3.3.9

RUN yum -y update && \
    yum install -y tar && \
    yum install -y lsof && \
    yum clean all

RUN mkdir -p /usr/share/maven \
    && curl -fsSL http://mirrors.sonic.net/apache/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz \
       | tar -xzC /usr/share/maven --strip-components=1 \
    && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn

ENV MAVEN_HOME /usr/share/maven

VOLUME /root/.m2

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

# Solr/Jetty web application
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

EXPOSE 8983

# FILE MANAGER
ADD filemgr.properties $OODT_HOME/cas-filemgr/etc/filemgr.properties

ENTRYPOINT ["/usr/local/oodt/start.sh"]
