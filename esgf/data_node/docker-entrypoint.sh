#!/bin/bash

# configure TDS web.xml
echo "Configuring TDS to use: ESGF_HOSTNAME=${ESGF_HOSTNAME}"
sed -i 's/my\.esgf\.node/'"${ESGF_HOSTNAME}"'/g' /usr/local/tomcat/webapps/thredds/WEB-INF/web.xml

# start Tomcat, keep it running
/usr/local/tomcat/bin/catalina.sh run
