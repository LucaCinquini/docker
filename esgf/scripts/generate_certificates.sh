#!/bin/bash
# Script to generate certificates needed for an ESGF node
# All certificates are generated in the directory $ESGF_CONFIG/esgfcerts, then moved to the proper location under $ESGF_CONFIG
# Usage: ./generate_certificates.sh <FQDN>

if [ "${ESGF_CONFIG}" = "" ];
then
   echo "Env variable ESGF_CONFIG must be set  "
   exit -1
fi

hostname="$1"

# working directory
mkdir -p $ESGF_CONFIG/esgfcerts
cd $ESGF_CONFIG/esgfcerts
cp ../esg/config/tomcat/esg-truststore.ts ./esg-truststore.ts
cp ../httpd/certs/esgf-ca-bundle.crt-orig ./esgf-ca-bundle.crt

# generate host private key hostkey.pem, certificate hostcert.pem
echo ""
echo "Generating host certificate, key for $hostname"
openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout hostkey.pem -out hostcert.pem -subj "/C=/ST=/L=/O=ESGF/OU=/CN=$hostname"

# convert certificate to pkcs12 format
echo ""
echo "Converting certificate to pkcs12 format"
openssl pkcs12 -export -out hostcert.pkcs12 -in hostcert.pem -inkey hostkey.pem -password pass:changeit

# convert certificate to keystore
echo ""
echo "Converting to keystore format"
rm -f hostcert.jks
keytool -importkeystore -srckeystore hostcert.pkcs12 -srcstoretype pkcs12 -destkeystore hostcert.jks -srcstorepass changeit -deststorepass changeit
# change alias to 'my_esgf_node'
keytool -changealias -v -alias 1 -destalias my_esgf_node -keystore hostcert.jks -storepass changeit

# append to apache httpd cert chain (httpd must trust itself):
cat hostcert.pem >> esgf-ca-bundle.crt

# create certificate hash
# (pointed to by SSL_CERT_DIR, needed by coG for openid authentication)
echo ""
echo "Generating certificate hash"
cert_hash=`openssl x509 -noout -hash -in hostcert.pem`
cp hostcert.pem ${cert_hash}.0

# import self-signed certificate into ESGF truststore
echo ""
echo "Importing host certificate into ESGF trust-store"
# remove previous alias from truststore:
keytool -delete -alias my_esgf_node -keystore esg-truststore.ts -storepass changeit
# import apache self-signed cert into truststore
keytool -import -v -trustcacerts -noprompt  -alias my_esgf_node -keypass changeit -file hostcert.pem -keystore esg-truststore.ts -storepass changeit

# copy all certificates to their target location
# apache httpd
cp hostcert.pem $ESGF_CONFIG/httpd/certs/hostcert.pem
cp hostkey.pem $ESGF_CONFIG/httpd/certs/hostkey.pem
cp esgf-ca-bundle.crt $ESGF_CONFIG/httpd/certs/esgf-ca-bundle.crt
# cog, gridftp
cp hostcert.pem $ESGF_CONFIG/grid-security/hostcert.pem
cp hostkey.pem $ESGF_CONFIG/grid-security/hostkey.pem
cp ${cert_hash}.0 $ESGF_CONFIG/grid-security/certificates/${cert_hash}.0
# tomcat
cp esg-truststore.ts $ESGF_CONFIG/esg/config/tomcat/esg-truststore.ts
cp hostcert.jks $ESGF_CONFIG/esg/config/tomcat/keystore-tomcat

