# ESGF node with Tomcat configured for ESGF services:
# o ESGF configuration files located in /esg/config
# o ESGF keystore for "my.esgf.node"
# o ESGF trusstore
# o ESGF_HOME env

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

# override server.xml
COPY server.xml /usr/local/tomcat/conf/server.xml

# customized copy of context.xml that increases the Tomcat cache to avoid flood of warning messages
COPY context.xml /usr/local/tomcat/conf/context.xml

# necessary to locate /esg/config/esgf.properties
ENV ESGF_HOME /esg

# ESGF configuration
COPY esgf-config/ /esg/config/

# ESGF content
RUN mkdir /esg/content

# Java truststore
# COPY esgf-config/tomcat/esg-truststore.ts /usr/java/latest/jre/lib/security/jssecacerts

VOLUME /esg/config
VOLUME /esg/content

EXPOSE 8080
EXPOSE 8009
EXPOSE 8443

# custom env variables for starting Tomcat
COPY setenv.sh $CATALINA_HOME/bin

ENTRYPOINT ["/usr/local/tomcat/bin/catalina.sh", "run"]
