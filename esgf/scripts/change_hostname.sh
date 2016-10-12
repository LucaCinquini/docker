#!/bin/bash
# script to change the ESGF node hostname
# among other things, this script changes several configuration files in the directory tree under $ESGF_CONFIG

if [ "${ESGF_CONFIG}" = "" ];
then
   echo "Env variable ESGF_CONFIG must be set  "
   exit -1
fi

hostname="$1"
echo "Changing hostname to: $hostname"

# change common ESGF configuration files
sed -i -- 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/esg/config/esgf.properties
sed -i -- 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/esg/config/esgf_idp_static.xml

# change apache httpd configuration
sed -i -- 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/httpd/conf/esgf-httpd.conf

# change CoG initial settings (used to seed cog_settings.cfg)
sed -i -- 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/cog/cog_config/esgf-cog_settings.cfg
#sed -i -- 's/my\.esgf\.node/'"${hostname}"'/g' $ESGF_CONFIG/cog/cog_config/cog_settings.cfg

