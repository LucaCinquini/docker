# EDRN LabCAS back-end services
# Image includes test workflow and data upload.

FROM centos:centos6.7

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>


# Update to last version
RUN yum -y update; yum clean all

# pre-requisite: install Java 8
# Acnowldegment: https://github.com/Mashape/docker-java8/blob/master/Dockerfile

# install wget
RUN yum -y update && \
    yum -y install wget && \
    yum clean all

# Downloading Java
ENV JAVA_VERSION 8u31
ENV BUILD_VERSION b13
RUN wget --no-cookies --no-check-certificate --header "Cookie: oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/$JAVA_VERSION-$BUILD_VERSION/jdk-$JAVA_VERSION-linux-x64.rpm" -O /tmp/jdk-8-linux-x64.rpm

RUN yum -y install /tmp/jdk-8-linux-x64.rpm && \
    rm /tmp/jdk-8-linux-x64.rpm

RUN alternatives --install /usr/bin/java jar /usr/java/latest/bin/java 200000
RUN alternatives --install /usr/bin/javaws javaws /usr/java/latest/bin/javaws 200000
RUN alternatives --install /usr/bin/javac javac /usr/java/latest/bin/javac 200000

ENV JAVA_HOME /usr/java/latest

# pre-requisite: install Git

RUN yum -y update && \
    yum -y install git && \
    yum clean all && \
    git --version

# pre-requisite: install Maven
# Acnowldegment: https://github.com/carlossg/docker-maven/blob/993474f006a59f487ef795864fab6115f9b1b8b5/jdk-8/Dockerfile

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

# install LabCAS

ENV LABCAS_BACKEND_TAG v0.7.2

# define LabCAS environment

# location where all software components will be installed
ENV LABCAS_HOME /usr/local/labcas/home
RUN mkdir -p $LABCAS_HOME

# root location where products will be uploaded (only needed to support publishing of uploaded files)
ENV LABCAS_STAGING /usr/local/labcas/staging
run mkdir -p $LABCAS_STAGING
 
# location where product files will be archived
ENV LABCAS_ARCHIVE /usr/local/labcas/archive
RUN mkdir -p $LABCAS_ARCHIVE
 
# location where the specific executable will be installed (possibly outside of LabCAS version control)
ENV PGE_ROOT /usr/local/labcas/backend/pges
RUN mkdir -p $PGE_ROOT

# location of source code
ENV LABCAS_SRC /usr/local/labcas/src
RUN mkdir -p $LABCAS_SRC

# download labcas-backend
RUN cd $LABCAS_SRC && \
       git clone https://github.com/EDRN/labcas-backend.git && \
       cd labcas-backend && \
       git checkout $LABCAS_BACKEND_TAG

# install all back-end services to $LABCAS_HOME
RUN cd $LABCAS_SRC/labcas-backend && \ 
       mvn install && \
       mvn install -Dworkflow=labcas-test && \
       mvn install -Dworkflow=labcas-upload

# expose Solr, OODT ports
EXPOSE 8983
EXPOSE 9000
EXPOSE 9001
EXPOSE 9002

# default working directory
WORKDIR $LABCAS_HOME

# create non-privileged user to run services
RUN groupadd -r labadmin && \
    useradd -r -g labadmin labadmin && \
    mkdir -p ~labadmin

# change user prompt
RUN echo 'export PS1="[\u@\h]\$ "' >> ~labadmin/.bashrc    

# change ownership of application directories
RUN chown -R labadmin:labadmin $LABCAS_HOME && \
    chown -R labadmin:labadmin $LABCAS_STAGING && \
    chown -R labadmin:labadmin $LABCAS_ARCHIVE && \
    chown -R labadmin:labadmin $PGE_ROOT && \
    chown -R labadmin:labadmin $LABCAS_SRC

# expose application data (must do this AFTER chown)
VOLUME $LABCAS_HOME
VOLUME $LABCAS_STAGING
VOLUME $LABCAS_ARCHIVE
VOLUME $PGE_ROOT
VOLUME $LABCAS_SRC

# default user
USER labadmin

# default working directory
WORKDIR $LABCAS_HOME
