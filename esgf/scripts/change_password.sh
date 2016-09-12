#!/bin/bash
# script to change the root ESGF password
# the new password is obtained from the env variable ESGF_PASSWORD

if [ "$ESGF_PASSWORD" = "" ]
then
   echo "Env variable ESGF_PASSWORD is not set, exiting"
   exit -1
else
   echo "Env variable ESGF_PASSWORD is set"
fi

# change password inside esgf_postgres container
docker exec -it esgf_postgres /bin/bash -c "export ESGF_PASSWORD=${ESGF_PASSWORD} && /usr/share/bin/change_password.sh"
