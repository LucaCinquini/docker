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

# override server.xml
COPY server.xml  /usr/local/tomcat/conf/server.xml

# customized copy of context.xml that increases the Tomcat cache to avoid flood of warning messages
COPY context.xml /usr/local/tomcat/conf/context.xml

# necessary to locate /esg/config/esgf.properties
ENV ESGF_HOME /esg
# ESGF configuration
COPY esgf-config/   /esg/config/

VOLUME /esg/config

EXPOSE 8080
EXPOSE 8009
EXPOSE 8443

# FIXME
RUN mkdir -p /usr/local/tomcat/webapps/esg-orp
COPY esg-orp.war /usr/local/tomcat/webapps/esg-orp/esg-orp.war
RUN cd /usr/local/tomcat/webapps/esg-orp && \
    jar xvf esg-orp.war && \
    rm esg-orp.war

# properties to read the Tomcat kesytore, used to sign the authentication cookie
COPY esg-orp.properties /usr/local/tomcat/webapps/esg-orp/WEB-INF/classes/esg-orp.properties

# TDS
RUN mkdir -p /usr/local/tomcat/webapps/thredds
COPY thredds.war /usr/local/tomcat/webapps/thredds/thredds.war
RUN cd /usr/local/tomcat/webapps/thredds && \
    jar xvf thredds.war && \
    rm thredds.war

# ESGF configuration
# directory containing logs and catalogs
#RUN mkdir -p /esg/content

# memory configuration
COPY esgf-threddsConfig.xml /esg/content/thredds/threddsConfig.xml

# customized applicationContext.xml file with ESGF authorizer
COPY esgf-applicationContext.xml /usr/local/tomcat/webapps/thredds/WEB-INF/applicationContext.xml

# customized web.xml file with ESGF security filters
COPY esgf-web.xml /usr/local/tomcat/webapps/thredds/WEB-INF/web.xml

# jars necessary to support ESGF security filters
COPY  jars/*  $CATALINA_HOME/webapps/thredds/WEB-INF/lib/

# customized logging
COPY log4j2.xml /usr/local/tomcat/webapps/thredds/WEB-INF/classes/log4j2.xml

# test ESGF catalogs
#COPY esgf-content/thredds/ /esg/content/thredds/

# Java truststore
COPY esgf-config/tomcat/esg-truststore.ts /usr/java/latest/jre/lib/security/jssecacerts

COPY setenv.sh $CATALINA_HOME/bin

#ENTRYPOINT ["/usr/local/tomcat/bin/catalina.sh", "run"]
