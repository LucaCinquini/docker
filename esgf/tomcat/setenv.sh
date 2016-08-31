#!/bin/bash
# script containing env variables for TDS context

# TDS + ORP
#export JAVA_OPTS="-Dtds.content.root.path=/esg/content -Djavax.net.debug=ssl"
export JAVA_OPTS="-Dtds.content.root.path=/esg/content -Djavax.net.debug=record,keygen,handshake"
export CATALINA_OPTS="-Xmx2048m -server -Xms1024m -XX:MaxPermSize=512m  -Dsun.security.ssl.allowUnsafeRenegotiation=false -Djavax.net.ssl.trustStore='/esg/config/tomcat/esg-truststore.ts' -Djavax.net.ssl.trustStorePassword='changeit'"
