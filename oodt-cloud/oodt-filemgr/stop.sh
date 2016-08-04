#!/bin/bash

# stop OODT File Manager
cd $OODT_HOME/cas-filemgr/bin
./filemgr stop

# stop 
cd $SOLR_INSTALL_DIR/bin
./solr stop

cd $OODT_HOME
