#!/bin/bash
# script to change the ESGF password in the CoG container:
# - for access to the postgres databases (CoG+ESGF)
# - for web authentication of the rootAdmin user

if [ "${ESGF_PASSWORD}" = "" ] || [ "${COG_CONFIG_DIR}" = "" ];
then
   echo "All env variables: ESGF_PASSWORD, COG_CONFIG_DIR must be set  "
   exit -1
fi

# first change the password for web-authentication of rootAdmin user
# (uses the old password in cog_settings.py to access the database)
source /usr/local/cog/venv/bin/activate
cd /usr/local/cog/cog_install
python manage.py change_password 'https://my.esgf.node/esgf-idp/openid/rootAdmin' "${ESGF_PASSWORD}"

# then change password to access the postgres databases in CoG settings file
sed -i -- 's/DATABASE_PASSWORD = .*/DATABASE_PASSWORD = '"${ESGF_PASSWORD}"'/g' $COG_CONFIG_DIR/cog_settings.cfg
sed -i -- 's/dbsuper:.*@esgf_postgres_app/dbsuper:'"${ESGF_PASSWORD}"'@esgf_postgres_app/g' $COG_CONFIG_DIR/cog_settings.cfg
