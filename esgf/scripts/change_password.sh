#!/bin/bash
# script to change the root ESGF password
# the new password is obtained from the env variable ESGF_PASSWORD
# most password changes are executed directly inside each container

if [ "${ESGF_PASSWORD}" = "" ] || [ "${ESGF_HOME}" = "" ];
then
   echo "All env variables: ESGF_PASSWORD, ESGF_HOME must be set  "
   exit -1
fi

# change password in common ESGF configuration files under $ESGF_HOME
echo ${ESGF_PASSWORD} > ${ESGF_HOME}/config/.esg_pg_pass
echo ${ESGF_PASSWORD} > ${ESGF_HOME}/config/.esgf_pass
sed -i -- 's/db.password=.*/db.password='"${ESGF_PASSWORD}"'/g' ${ESGF_HOME}/config/esgf.properties

# must first change cog password inside esgf_httpd container
# before the postgres password changes
docker exec -it esgf_httpd /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/share/bin/change_cog_password.sh"

# change password inside esgf_postgres container
docker exec -it esgf_postgres /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/share/bin/change_postgres_password.sh"

# change password inside esgf-data-node container
docker exec -it esgf-data-node /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/share/bin/change_data_node_password.sh"
