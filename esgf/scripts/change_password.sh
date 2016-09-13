#!/bin/bash
# script to change the root ESGF password
# the new password is obtained from the env variable ESGF_PASSWORD
# files to be changed are located under $ESGF_HOME/config, $COG_CONFIG_DIR

# check for required environment
# FIXME: consolidate
if [ $ESGF_PASSWORD == "" ];
then
   echo "Env variable ESGF_PASSWORD not set, exiting"
   exit -1
fi
if [ $ESGF_HOME == "" ];
then
   echo "Env variable ESGF_HOME not set, exiting"
   exit -1
fi
if [ $COG_CONFIG_DIR == "" ];
then
   echo "Env variable COG_CONFIG_DIR not set, exiting"
   exit -1
fi

# change password inside esgf_postgres container
docker exec -it esgf_postgres /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/share/bin/change_postgres_password.sh"

# change password inside esgf-data-node container
docker exec -it esgf-data-node /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/share/bin/change_data_node_password.sh"

# change password in common ESGF configuration files under $ESGF_HOME
# FIXME: run inside container
echo ${ESGF_PASSWORD} > ${ESGF_HOME}/config/.esg_pg_pass
echo ${ESGF_PASSWORD} > ${ESGF_HOME}/config/.esgf_pass

# change password in CoG settings file
# FIXME: run inside httpd container
sed -i -- 's/DATABASE_PASSWORD = .*/DATABASE_PASSWORD = '"${ESGF_PASSWORD}"'/g' $COG_CONFIG_DIR/cog_settings.cfg
sed -i -- 's/dbsuper:.*@esgf_postgres_app/dbsuper:'"${ESGF_PASSWORD}"'@esgf_postgres_app/g' $COG_CONFIG_DIR/cog_settings.cfg
