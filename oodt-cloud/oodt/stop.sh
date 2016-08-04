#!/bin/bash

# Stop all OODT backend services

# CAS File Manager
cd $OODT_HOME/cas-filemgr/bin
./filemgr stop

# CAS Workflow Manager
cd $OODT_HOME/cas-workflow/bin
./wmgr stop

# CAS Resource Manager
cd $OODT_HOME/cas-resource/bin
./resmgr stop

# Solr
cd $SOLR_INSTALL_DIR/solr/bin
./solr stop

cd $OODT_HOME
