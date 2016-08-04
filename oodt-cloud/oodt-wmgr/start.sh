#!/bin/bash

# OODT Workflow Manager
cd $OODT_HOME/cas-workflow/bin
./wmgr start

# keep container running
tail -f $OODT_HOME/cas-workflow/logs/cas_workflow0.log
