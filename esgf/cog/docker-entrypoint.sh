#!/bin/bash

# command line arguments

# DOCKER_IP=192.168.99.100
export DOCKER_IP=$1
echo "DOCKER_IP=$DOCKER_IP"

# esgf_flag=false/true
export ESGF_FLAG=$2
echo "ESGF_FLAG=$ESGF_FLAG"

# runserver=true/false
export RUNSERVER=$3
echo "RUNSERVER=$RUNSERVER"

# use virtualenv
source $COG_DIR/venv/bin/activate

# initialize COG settings with ESGF settings
if [ -f "${COG_CONFIG_DIR}/esgf-cog_settings.cfg" ]; then
   if [ ! -f "${COG_CONFIG_DIR}/cog_settings.cfg" ]; then
     cp ${COG_CONFIG_DIR}/esgf-cog_settings.cfg ${COG_CONFIG_DIR}/cog_settings.cfg
   fi
fi

# upgrade CoG
cd $COG_INSTALL_DIR
python setup.py -q setup_cog --esgf=$ESGF_FLAG

# customize CoG settings
echo "Using DOCKER_IP=$DOCKER_IP"
sed -i 's/ALLOWED_HOSTS = .*/ALLOWED_HOSTS = '"${DOCKER_IP}"'/g' $COG_CONFIG_DIR/cog_settings.cfg

# PRODUCTION_SERVER = True would require use of SSL to transmit any cookie
sed -i 's/PRODUCTION_SERVER = True/PRODUCTION_SERVER = False/g' $COG_CONFIG_DIR/cog_settings.cfg

# start CoG in virtual environment
$RUNSERVER && python ./manage.py runserver 0.0.0.0:8000
