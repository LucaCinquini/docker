#!/bin/bash

# start Solr
cd $SOLR_INSTALL_DIR/bin
./solr start -p 8983 -s $SOLR_HOME -d $SOLR_INSTALL_DIR/server -a '-Dsolr.data.dir=$SOLR_DATA_DIR'

# keep container running
tail -f $SOLR_INSTALL_DIR/server/logs/solr.log
