#!/bin/bash

# stop Tomcat
cd $CATALINA_HOME/bin
./catalina.sh stop

# stop Solrs
cd $SOLR_INSTALL_DIR/bin
./solr stop -all

# stop Apache httpd
service httpd stop
