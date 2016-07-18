#!/bin/bash

# use virtualenv
source $COG_DIR/venv/bin/activate

# start postgres
su -c 'pg_ctl start -D /var/lib/pgsql/data' postgres

# upgrade CoG
cd $COG_INSTALL_DIR
python setup.py -q setup_cog --esgf=true

# customize CoG settings
export DOCKER_IP=$@
echo "Using DOCKER_IP=$DOCKER_IP"
sed -i 's/ALLOWED_HOSTS = .*/ALLOWED_HOSTS = '"${DOCKER_IP}"'/g' $COG_CONFIG_DIR/cog_settings.cfg

# PRODUCTION_SERVER = True would require use of SSL to transmit any cookie
sed -i 's/PRODUCTION_SERVER = True/PRODUCTION_SERVER = False/g' $COG_CONFIG_DIR/cog_settings.cfg

# Start CoG in virtual environment
cd $COG_INSTALL_DIR
python ./manage.py runserver 0.0.0.0:8000
