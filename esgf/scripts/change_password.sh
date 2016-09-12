#!/bin/bash
# script to change the root ESGF password
# the new password is obtained from the env variable ESGF_PASSWORD
# files to be changed are located under $ESGF_HOME/config

# check for required environment
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

# change password inside esgf_postgres container
docker exec -it esgf_postgres /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/share/bin/change_password.sh"

# change password in common ESGF configuration files under $ESGF_HOME
echo ${ESGF_PASSWORD} > ${ESGF_HOME}/config/.esg_pg_pass
echo ${ESGF_PASSWORD} > ${ESGF_HOME}/config/.esgf_pass
