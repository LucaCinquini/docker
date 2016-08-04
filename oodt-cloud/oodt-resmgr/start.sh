#!/bin/bash

# OODT Resource Manager
cd $OODT_HOME/cas-resource/bin
./resmgr start

# keep script running
cd $OODT_HOME
tail -f $OODT_HOME/cas-resource/logs/cas_resource0.log
