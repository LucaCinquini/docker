#!/bin/bash
# script to change the ESGF node hostname
# among other things, this script changes several configuration files in the directory tree under $ESGF_CONFIG
# Usage: ./change_hostname.sh <FQDN>
# Example: ./change_hostname.sh docker-node.esgf.org


if [ "${ESGF_CONFIG}" = "" ];
then
   echo "Env variable ESGF_CONFIG must be set  "
   exit -1
fi

hostname="$1"
echo "Changing hostname to: $hostname"

# change common ESGF configuration files
sed -i .back 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/esg/config/esgf.properties
sed -i .back 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/esg/config/esgf_idp_static.xml
sed -i .back 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/esg/config/esgf_shards_static.xml

# change apache httpd configuration
sed -i .back 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/httpd/conf/esgf-httpd.conf

# change CoG settings
sed -i .back 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/cog/cog_config/cog_settings.cfg


# change TDS access control filters
sed -i .back 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/webapps/thredds/WEB-INF/web.xml
