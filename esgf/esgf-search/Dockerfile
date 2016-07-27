FROM esgfhub/esgf-tomcat

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# ESGF esg-search web application
# must have version 4.9.2+
COPY esg-search.war /usr/local/tomcat/webapps
COPY esgf.properties /esg/config/esgf.properties
COPY esgf_shards_static.xml /esg/config/esgf_shards_static.xml
COPY esgf_shards.xml /esg/config/esgf_shards.xml

ENTRYPOINT ["/usr/local/tomcat/bin/catalina.sh", "run"]
