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

echo ""
echo "----------------------------------"
echo "Currently running services:"
echo "----------------------------------"
ps -ef | grep oodt
ps -ef | grep catalina
ps -ef | grep solr

# keep script running
cd $OODT_HOME
tail -f $OODT_HOME/cas-workflow/logs/cas_workflow0.log
