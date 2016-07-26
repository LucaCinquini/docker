FROM centos:centos6.7

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>


# Update to last version
RUN yum -y update; yum clean all

# install utilities
RUN yum -y update && \
    yum -y install wget tar lsof && \
    yum clean all

# pre-requisite: install Java 8
# Acnowldegment: https://github.com/Mashape/docker-java8/blob/master/Dockerfile
# Oracle Java 8
ENV JAVA_VERSION 8u31
ENV BUILD_VERSION b13
RUN wget --no-cookies --no-check-certificate --header "Cookie: oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/$JAVA_VERSION-$BUILD_VERSION/jdk-$JAVA_VERSION-linux-x64.rpm" -O /tmp/jdk-8-linux-x64.rpm
RUN yum -y install /tmp/jdk-8-linux-x64.rpm && \
    rm /tmp/jdk-8-linux-x64.rpm
RUN alternatives --install /usr/bin/java jar /usr/java/latest/bin/java 200000
RUN alternatives --install /usr/bin/javaws javaws /usr/java/latest/bin/javaws 200000
RUN alternatives --install /usr/bin/javac javac /usr/java/latest/bin/javac 200000
ENV JAVA_HOME /usr/java/latest

# apache httpd
RUN yum install -y httpd
COPY localhost.conf /etc/httpd/conf.d/localhost.conf

# Tomcat 8
ENV TOMCAT_VERSION 8.5.4

RUN wget -O /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz http://mirror.reverse.net/pub/apache/tomcat/tomcat-8/v${TOMCAT_VERSION}/bin/apache-tomcat-${TOMCAT_VERSION}.tar.gz && \
    cd /usr/local && tar xzf /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz && \
    ln -s /usr/local/apache-tomcat-${TOMCAT_VERSION} /usr/local/tomcat && \
    rm /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz
ENV CATALINA_HOME /usr/local/tomcat

# Solr/Jetty web application
ENV SOLR_VERSION 5.3.1
ENV SOLR_INSTALL_DIR /usr/local/solr
ENV SOLR_HOME /usr/local/solr-home
ENV SOLR_DATA_DIR /esg/solr-index

RUN wget -O /tmp/solr-${SOLR_VERSION}.tgz http://archive.apache.org/dist/lucene/solr/${SOLR_VERSION}/solr-${SOLR_VERSION}.tgz
RUN cd /usr/local && \
    tar xzf /tmp/solr-${SOLR_VERSION}.tgz && \
    rm /tmp/solr-${SOLR_VERSION}.tgz && \
    ln -s ./solr-${SOLR_VERSION} ./solr
RUN mkdir -p $SOLR_DATA_DIR
RUN cd /tmp && \
    wget -O solr-home.tar http://distrib-coffee.ipsl.jussieu.fr/pub/esgf/dist/esg-search/solr-home.tar && \
    tar xvf /tmp/solr-home.tar
RUN mkdir -p $SOLR_HOME

# add shards
COPY solr/add_shard.sh /usr/local/bin/add_shard.sh
RUN /usr/local/bin/add_shard.sh master 8984
RUN /usr/local/bin/add_shard.sh slave 8983

# ESGF esg-search web application
# must have version 4.9.2+
COPY esg-search.war /usr/local/tomcat/webapps
COPY esgf.properties /esg/config/esgf.properties
COPY esgf_shards_static.xml /esg/config/esgf_shards_static.xml
COPY esgf_shards.xml /esg/config/esgf_shards.xml

COPY ./start.sh /usr/local/bin/start.sh
COPY ./stop.sh /usr/local/bin/stop.sh

COPY ./docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["localhost"]
