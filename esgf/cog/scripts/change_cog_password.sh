#!/bin/bash
# script to change the ESGF password in the CoG container:
# - for access to the postgres databases (CoG+ESGF)
# - for web authentication of the rootAdmin user

if [ "${ESGF_PASSWORD}" = "" ] || [ "${ESGF_CONFIG}" = "" ] || [ "${ESGF_HOSTNAME}" = "" ];
then
   echo "All env variables: ESGF_PASSWORD, ESGF_CONFIG must be set  "
   exit -1
fi

# first change the password for web-authentication of rootAdmin user
# (uses the old password in cog_settings.py to access the database)
source /usr/local/cog/venv/bin/activate
cd /usr/local/cog/cog_install
python manage.py change_password "https://${ESGF_HOSTNAME}/esgf-idp/openid/rootAdmin" "${ESGF_PASSWORD}"

# then change password to access the postgres databases in CoG settings file
sed -i -- 's/DATABASE_PASSWORD = .*/DATABASE_PASSWORD = '"${ESGF_PASSWORD}"'/g' $ESGF_CONFIG/cog/cog_config/cog_settings.cfg
sed -i -- 's/dbsuper:.*@esgf-postgres/dbsuper:'"${ESGF_PASSWORD}"'@esgf-postgres/g' $ESGF_CONFIG/cog/cog_config/cog_settings.cfg
