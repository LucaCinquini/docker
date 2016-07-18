#!/bin/bash

cd $LABCAS_HOME
./start.sh

# start process to keep the Docker container running
while true; do sleep 1000; done
