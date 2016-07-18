#!/bin/bash

# customize CoG settings
export DOCKER_IP=$@
sed -i 's/ALLOWED_HOSTS = localhost/ALLOWED_HOSTS = '"${DOCKER_IP}"', localhost/g' $COG_CONFIG_DIR/cog_settings.cfg

# Start CoG in virtual environment
source $COG_DIR/venv/bin/activate
cd $COG_INSTALL_DIR
python ./manage.py runserver 0.0.0.0:8000
