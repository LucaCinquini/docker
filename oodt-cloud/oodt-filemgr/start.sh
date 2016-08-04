#!/bin/bash

# start OODT File Manager
cd $OODT_HOME/cas-filemgr/bin
./filemgr start

# start Solr
cd $SOLR_INSTALL_DIR/bin
./solr start -p 8983 -s $SOLR_HOME -d $SOLR_INSTALL_DIR/server -a '-Dsolr.data.dir=$SOLR_DATA_DIR'

# keep script running
#tail -f $OODT_HOME/cas-workflow/logs/cas_workflow0.log
tail -f $SOLR_INSTALL_DIR/server/logs/solr.log
