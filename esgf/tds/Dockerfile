# Thredds Data Server packages as Docker container for ESGF
FROM esgfhub/esgf-tomcat

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

ENV TDS_VERSION 5.0.0

RUN mkdir -p /usr/local/tomcat/webapps/thredds
COPY thredds.war /usr/local/tomcat/webapps/thredds/thredds.war
RUN cd /usr/local/tomcat/webapps/thredds && \
    jar xvf thredds.war && \
    rm thredds.war

#COPY esgf.properties /esg/config/esgf.properties
#COPY esgf_shards_static.xml /esg/config/esgf_shards_static.xml
#COPY esgf_shards.xml /esg/config/esgf_shards.xml

# ESGF configuration
# directory containint logs and catalogs
RUN mkdir -p /esg/content
# jars necessary to support ESGF security filters
COPY jars/* $CATALINA_HOME/webapps/thredds/WEB-INF/lib/
# memory configuration
COPY esgf-threddsConfig.xml /esg/content/thredds/threddsConfig.xml
# customized applicationContext.xml file with ESGF authorizer
COPY esgf-applicationContext.xml /usr/local/tomcat/webapps/thredds/WEB-INF/applicationContext.xml 
# customized web.xml file with ESGF security filters
#COPY esgf-web.xml /usr/local/tomcat/webapps/thredds/WEB-INF/web.xml

# FIXME
RUN mkdir -p /usr/local/tomcat/webapps/esg-orp
COPY esg-orp.war /usr/local/tomcat/webapps/esg-orp/esg-orp.war
RUN cd /usr/local/tomcat/webapps/esg-orp && \
    jar xvf esg-orp.war && \
    rm esg-orp.war

# properties to read the Tomcat kesytore, used to sign the authentication cookie
COPY esg-orp.properties /usr/local/tomcat/webapps/esg-orp/WEB-INF/classes/esg-orp.properties

COPY setenv.sh $CATALINA_HOME/bin

ENTRYPOINT ["/usr/local/tomcat/bin/catalina.sh", "run"]
