#!/bin/bash

# CAS File Manager
cd $OODT_HOME/cas-filemgr/bin
./filemgr stop

# CAS Workflow Manager
cd $OODT_HOME/cas-workflow/bin
./wmgr stop

# CAS Resource Manager
cd $OODT_HOME/cas-resource/bin
./resmgr stop

cd $OODT_HOME
