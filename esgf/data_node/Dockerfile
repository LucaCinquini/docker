# ESGF data node
# o TDS customized with ESGF access control filters
# o ORP

FROM esgfhub/esgf-tomcat

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# ESGF ORP
RUN mkdir -p /usr/local/tomcat/webapps/esg-orp
COPY esgf-orp/esg-orp.war /usr/local/tomcat/webapps/esg-orp/esg-orp.war
RUN cd /usr/local/tomcat/webapps/esg-orp && \
    jar xvf esg-orp.war && \
    rm esg-orp.war

# properties to read the Tomcat kesytore, used to sign the authentication cookie
COPY esgf-orp/esg-orp.properties /usr/local/tomcat/webapps/esg-orp/WEB-INF/classes/esg-orp.properties

# TDS
RUN mkdir -p /usr/local/tomcat/webapps/thredds
COPY thredds/thredds.war /usr/local/tomcat/webapps/thredds/thredds.war
RUN cd /usr/local/tomcat/webapps/thredds && \
    jar xvf thredds.war && \
    rm thredds.war

# TDS memory configuration
COPY thredds/esgf-threddsConfig.xml /esg/content/thredds/threddsConfig.xml

# TDS customized applicationContext.xml file with ESGF authorizer
COPY thredds/esgf-applicationContext.xml /usr/local/tomcat/webapps/thredds/WEB-INF/applicationContext.xml

# TDS customized web.xml file with ESGF security filters
COPY thredds/esgf-web.xml /usr/local/tomcat/webapps/thredds/WEB-INF/web.xml

# TDS jars necessary to support ESGF security filters
COPY thredds/esgf-jars/*  $CATALINA_HOME/webapps/thredds/WEB-INF/lib/

# TDS customized logging (uses DEBUG)
COPY thredds/esgf-log4j2.xml /usr/local/tomcat/webapps/thredds/WEB-INF/classes/log4j2.xml

# data node scripts
COPY scripts/ /usr/share/bin/
RUN chmod +x /usr/share/bin/change_data_node_password.sh

# TDS test ESGF catalogs
# COPY esgf-content/thredds/ /esg/content/thredds/

ENTRYPOINT ["/usr/local/tomcat/bin/catalina.sh", "run"]
#ENTRYPOINT ["tail", "-f", "/dev/null"]
