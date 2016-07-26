#!/bin/bash

# use Docker Host IP address
export DOCKER_IP=$@
echo "Using DOCKER_IP=$DOCKER_IP"
sed -i 's/localhost/'"${DOCKER_IP}"'/g' /esg/config/esgf.properties

# start Tomcat
cd $CATALINA_HOME/bin
./catalina.sh start

# start Solrs
cd $SOLR_INSTALL_DIR/bin
export SOLR_INCLUDE=${SOLR_HOME}/solr.in.sh
./solr start -d $SOLR_INSTALL_DIR/server -s $SOLR_HOME/master-8984 -p 8984 -a '-Denable.master=true'
./solr start -d $SOLR_INSTALL_DIR/server -s $SOLR_HOME/slave-8983 -p 8983 -a '-Denable.slave=true'

# start Apache httpd
service httpd restart

# keep container running
tail -f $CATALINA_HOME/logs/catalina.out
