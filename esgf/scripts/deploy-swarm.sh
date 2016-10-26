#!/bin/sh

docker network create -d overlay db-network
docker network create -d overlay esgf-network

# assign labels to nodes
docker node update --label-add esgf_type=front-end acce-build1.dyndns.org
docker node update --label-add esgf_type=data-node acce-build2.dyndns.org
docker node update --label-add esgf_type=index-node acce-build3.dyndns.org

# create volumes for ESGF configuration and data
#export ESGF_CONFIG=/usr/local/adeploy/esgf/acce1
#docker service create --replicas 1 --name esgf-config-front-end --network esgf-network --constraint 'node.labels.esgf_type==front-end' \
#                      --mount type=bind,source=$ESGF_CONFIG/httpd/certs/,destination=/etc/certs/ \
#                      --mount type=bind,source=$ESGF_CONFIG/httpd/conf/esgf-httpd.conf,destination=/etc/httpd/conf.d/esgf-httpd.conf \
#                      --mount type=bind,source=$ESGF_CONFIG/grid-security/certificates/,destination=/etc/grid-security/certificates/ \
#                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,destination=/esg/config/ \
#                      --mount type=bind,source=$ESGF_CONFIG/webapps/thredds/WEB-INF/web.xml,destination=/usr/local/tomcat/webapps/thredds/WEB-INF/web.xml \
#                      --constraint 'node.labels.esgf_type==front-end' centos:centos6.7 tail -f /dev/null

# start 'postgres' service
docker service create --replicas 1 --name esgf-postgres -p 5432:5432 --network db-network  --constraint 'node.labels.esgf_type==front-end' esgfhub/esgf-postgres

# start 'idp' service
docker service create --replicas 1 --name esgf-idp-node -p 8445:8443 --network esgf-network --network db-network  \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,destination=/esg/config/ \
                      --constraint 'node.labels.esgf_type==front-end' esgfhub/esgf-idp-node

# start 'cog' container
# FIXME: keep the container running without the django server
docker service create --replicas 1 --name esgf-cog --network esgf-network --network db-network \
                      --mount type=volume,source=cog_install_dir,destination=/usr/local/cog/cog_install/ \
                      --mount type=volume,source=cog_venv_dir,destination=/usr/local/cog/venv/ \
                      --mount type=bind,source=$ESGF_CONFIG/cog/cog_config/,destination=/usr/local/cog/cog_config/ \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,destination=/esg/config/ \
                      --constraint 'node.labels.esgf_type==front-end' esgfhub/esgf-cog \
                      $ESGF_HOSTNAME true true

# start 'httpd' service
docker service create --replicas 1 --name esgf-httpd -p 80:80 -p 443:443 --network esgf-network --network db-network   \
                      --mount type=volume,source=cog_install_dir,destination=/usr/local/cog/cog_install/ \
                      --mount type=volume,source=cog_venv_dir,destination=/usr/local/cog/venv/ \
                      --mount type=bind,source=$ESGF_CONFIG/cog/cog_config/,destination=/usr/local/cog/cog_config/ \
                      --mount type=bind,source=$ESGF_CONFIG/httpd/certs/,destination=/etc/certs/ \
                      --mount type=bind,source=$ESGF_CONFIG/httpd/conf/,destination=/etc/httpd/conf.d/ \
                      --mount type=bind,source=$ESGF_CONFIG/grid-security/certificates/,destination=/etc/grid-security/certificates/ \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,destination=/esg/config/ \
                      --constraint 'node.labels.esgf_type==front-end' esgfhub/esgf-httpd

# start 'data-node' service
docker service create --replicas 1 --name esgf-data-node -p 8080:8080 -p 8443:8443 --network db-network   \
                      --mount type=volume,source=tds_data_dir,destination=/esg/content/thredds/ \
                      --mount type=bind,source=$ESGF_CONFIG/grid-security/certificates/,destination=/etc/grid-security/certificates/ \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,destination=/esg/config/ \
                      --mount type=bind,source=$ESGF_CONFIG/webapps/thredds/WEB-INF/web.xml,destination=/usr/local/tomcat/webapps/thredds/WEB-INF/web.xml \
                      --constraint 'node.labels.esgf_type==data-node' esgfhub/esgf-data-node

# start solr
docker service create --replicas 1 --name esgf-solr -p 8983:8983 -p 8984:8984 --network db-network  \
                      --mount type=volume,source=solr_data,destination=/esg/solr-index \
                      --constraint 'node.labels.esgf_type==index-node' esgfhub/esgf-solr

# start esgf-index-node
docker service create --replicas 1 --name esgf-index-node -p 8081:8080 -p 8444:8443 --network db-network  \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,destination=/esg/config/ \
                      --constraint 'node.labels.esgf_type==index-node' esgfhub/esgf-index-node

