#!/bin/bash
# script containing env variables for TDS context

# TDS
export JAVA_OPTS="-Dtds.content.root.path=/esg/content -Djava.ext.dirs=/esg/config"
export CATALINA_OPTS="-Xmx2048m -server -Xms1024m -XX:MaxPermSize=512m  -Dsun.security.ssl.allowUnsafeRenegotiation=false"
