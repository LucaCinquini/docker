#!/bin/bash

# OODT File Manager
cd $OODT_HOME/cas-filemgr/bin
./filemgr start

# OODT Workflow Manager
cd $OODT_HOME/cas-workflow/bin
./wmgr start

# OODT Resource Manager
cd $OODT_HOME/cas-resource/bin
./resmgr start

cd $OODT_HOME

echo ""
echo "----------------------------------"
echo "Currently running services:"
echo "----------------------------------"
ps -ef | grep oodt
ps -ef | grep catalina
ps -ef | grep solr

# keep script running
#tail -f $OODT_HOME/cas-workflow/logs/cas_workflow0.log

# solr
cd $SOLR_INSTALL_DIR/bin
./solr start -p 8983 -s $SOLR_HOME -d $SOLR_INSTALL_DIR/server -a '-Dsolr.data.dir=$SOLR_DATA_DIR'
tail -f $SOLR_INSTALL_DIR/server/logs/solr.log
