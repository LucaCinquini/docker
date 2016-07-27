#!/bin/bash

__mod_user() {
usermod -G wheel postgres
}

__create_db() {
su --login postgres --command "/usr/bin/postgres -D /var/lib/pgsql/data -p 5432" &
sleep 10
ps aux 

# create super user
su --login - postgres --command "psql -c \"CREATE USER dbsuper with CREATEROLE superuser PASSWORD 'changeit';\""
# create CoG database
su --login - postgres --command "psql -c \"CREATE DATABASE cogdb;\""
# create ESGF database
su --login - postgres --command "psql -c \"CREATE DATABASE esgcet;\""
# load ESGF security schema
su --login - postgres --command "psql esgcet < /tmp/esgf_security.sql"
# list database users
su --login - postgres --command "psql -c \"\du;\""
}

# Call functions
__mod_user
__create_db
