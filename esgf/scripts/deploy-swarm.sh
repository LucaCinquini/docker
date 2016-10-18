#!/bin/sh

# start 'postgres' service
docker service create --replicas 1 --name esgf-postgres -p 5432:5432 --network swarm-network  --constraint 'node.labels.esgf_type==db' esgfhub/esgf-postgres

# start 'idp' service
docker service create --replicas 1 --name esgf-idp -p 8445:8443 --network swarm-network  \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,target=/esg/config/ \
                      --constraint 'node.labels.esgf_type==idp' esgfhub/esgf-idp

# start 'cog' container
docker service create --replicas 1 --name esgf-cog --network swarm-network  \
                      --mount type=volume,src=cog_dir,dst=/usr/local/cog/ \
                      --mount type=bind,source=$ESGF_CONFIG/cog/cog_config/,target=/usr/local/cog/cog_config/ \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,target=/esg/config/ \
                      --constraint 'node.labels.esgf_type==front-end' esgfhub/esgf-cog \
                      $ESGF_HOSTNAME true true

# start 'httpd' service
docker service create --replicas 1 --name esgf-httpd -p 80:80 -p 443:443 --network swarm-network  \
                      --mount type=volume,src=cog_dir,dst=/usr/local/cog/ \
                      --mount type=bind,source=$ESGF_CONFIG/cog/cog_config/,target=/usr/local/cog/cog_config/ \
                      --mount type=bind,source=$ESGF_CONFIG/httpd/certs/,target=/etc/certs/ \
                      --mount type=bind,source=$ESGF_CONFIG/httpd/conf/,target=/etc/httpd/conf.d/ \
                      --mount type=bind,source=$ESGF_CONFIG/grid-security/certificates/,target=/etc/grid-security/certificates/ \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,target=/esg/config/ \
                      --constraint 'node.labels.esgf_type==front-end' esgfhub/esgf-httpd
