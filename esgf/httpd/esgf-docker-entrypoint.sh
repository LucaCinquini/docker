#!/bin/bash
# script to start httpd as part of ESGF services

# change CoG directory permission
chown -R apache:apache /usr/local/cog

# start httpd service
service httpd restart

# keep container running
tail -f /etc/httpd/logs/error_log
