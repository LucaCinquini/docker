#!/bin/bash

# command line arguments

# ESGF_HOSTNAME=.....
export ESGF_HOSTNAME=$1
echo "ESGF_HOSTNAME=$ESGF_HOSTNAME"

# esgf_flag=false/true
export ESGF_FLAG=$2
echo "ESGF_FLAG=$ESGF_FLAG"

# runserver=true/false
export RUNSERVER=$3
echo "RUNSERVER=$RUNSERVER"

# use virtualenv
source $COG_DIR/venv/bin/activate

# initialize COG settings with ESGF settings, if needed
if [ -f "${COG_CONFIG_DIR}/esgf-cog_settings.cfg" ]; then
   if [ ! -f "${COG_CONFIG_DIR}/cog_settings.cfg" ]; then
     cp ${COG_CONFIG_DIR}/esgf-cog_settings.cfg ${COG_CONFIG_DIR}/cog_settings.cfg
   fi
fi

# upgrade CoG
cd $COG_INSTALL_DIR
python setup.py -q setup_cog --esgf=$ESGF_FLAG

# customize CoG settings
echo "Using ESGF_HOSTNAME=$ESGF_HOSTNAME"
sed -i 's/ALLOWED_HOSTS = .*/ALLOWED_HOSTS = '"${ESGF_HOSTNAME}"'/g' $COG_CONFIG_DIR/cog_settings.cfg

# PRODUCTION_SERVER = True would require use of SSL to transmit any cookie
sed -i 's/PRODUCTION_SERVER = True/PRODUCTION_SERVER = False/g' $COG_CONFIG_DIR/cog_settings.cfg

# start django server in virtual environment
$RUNSERVER && python ./manage.py runserver 0.0.0.0:8000
