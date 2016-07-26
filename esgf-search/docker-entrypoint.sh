#!/bin/bash

# use Docker Host IP address
export DOCKER_IP=$@
echo "Using DOCKER_IP=$DOCKER_IP"
sed -i 's/localhost/'"${DOCKER_IP}"'/g' /esg/config/esgf.properties

# start services
/usr/local/bin/start.sh

# keep container running
tail -f $CATALINA_HOME/logs/catalina.out
