# ESGF node with Tomcat

FROM esgfhub/esgf-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# Tomcat 8
ENV TOMCAT_VERSION 8.5.4

RUN wget -O /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz http://mirror.reverse.net/pub/apache/tomcat/tomcat-8/v${TOMCAT_VERSION}/bin/apache-tomcat-${TOMCAT_VERSION}.tar.gz && \
    cd /usr/local && tar xzf /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz && \
    ln -s /usr/local/apache-tomcat-${TOMCAT_VERSION} /usr/local/tomcat && \
    rm /tmp/apache-tomcat-${TOMCAT_VERSION}.tar.gz
ENV CATALINA_HOME /usr/local/tomcat

# remove Tomcat example applications
RUN cd /usr/local/tomcat/webapps && \
    rm -rf docs examples host-manager manager

EXPOSE 8080
EXPOSE 8009

# customized copy of context.xml that increases the Tomcat cache to avoid flood of warning messages
COPY context.xml /usr/local/tomcat/conf/context.xml

ENTRYPOINT ["/usr/local/tomcat/bin/catalina.sh", "run"]
