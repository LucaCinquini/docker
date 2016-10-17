#!/bin/bash
# script to change the root ESGF password
# the new password is obtained from the env variable ESGF_PASSWORD
# some password changes are executed directly inside each container

if [ "${ESGF_PASSWORD}" = "" ] || [ "${ESGF_CONFIG}" = "" ];
then
   echo "All env variables: ESGF_PASSWORD, ESGF_CONFIG must be set  "
   exit -1
fi

# change password inside (running) postgres container
docker start postgres
# give postgres time to start
sleep 3
docker exec -it postgres /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/local/bin/change_password.sh"
docker stop postgres

# change password in common ESGF configuration files under $ESGF_CONFIG/esg/config
echo ${ESGF_PASSWORD} > ${ESGF_CONFIG}/esg/config/.esg_pg_pass
echo ${ESGF_PASSWORD} > ${ESGF_CONFIG}/esg/config/.esgf_pass
sed -i -- 's/db.password=.*/db.password='"${ESGF_PASSWORD}"'/g' ${ESGF_CONFIG}/esg/config/esgf.properties

# change password to access the postgres databases in CoG settings file
sed -i -- 's/DATABASE_PASSWORD = .*/DATABASE_PASSWORD = '"${ESGF_PASSWORD}"'/g' $ESGF_CONFIG/cog/cog_config/cog_settings.cfg
sed -i -- 's/dbsuper:.*@esgf-postgres/dbsuper:'"${ESGF_PASSWORD}"'@esgf-postgres/g' $ESGF_CONFIG/cog/cog_config/cog_settings.cfg

# change password inside (running) data-node container
docker start data-node
docker exec -it data-node /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/local/bin/change_data_node_password.sh"
docker stop data-node
