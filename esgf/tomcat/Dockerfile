FROM node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# Tomcat 8
ENV TOMCAT_VERSION 8.5.4

RUN wget -O /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz http://mirror.reverse.net/pub/apache/tomcat/tomcat-8/v${TOMCAT_VERSION}/bin/apache-tomcat-${TOMCAT_VERSION}.tar.gz && \
    cd /usr/local && tar xzf /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz && \
    ln -s /usr/local/apache-tomcat-${TOMCAT_VERSION} /usr/local/tomcat && \
    rm /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz
ENV CATALINA_HOME /usr/local/tomcat

EXPOSE 8080
EXPOSE 8009

# ESGF esg-search web application
# must have version 4.9.2+
COPY esg-search.war /usr/local/tomcat/webapps
COPY esgf.properties /esg/config/esgf.properties
COPY esgf_shards_static.xml /esg/config/esgf_shards_static.xml
COPY esgf_shards.xml /esg/config/esgf_shards.xml

ENTRYPOINT ["/usr/local/tomcat/bin/catalina.sh", "run"]
