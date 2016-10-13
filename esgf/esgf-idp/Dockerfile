# ESGF Identity Provider
# Implemented as Java web servlet application running inside Tomcat container.

FROM esgfhub/esgf-tomcat

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# ESGF esg-idp web application
COPY esgf-idp.war /usr/local/tomcat/webapps

ENTRYPOINT ["/usr/local/tomcat/bin/catalina.sh", "run"]
