#!/bin/sh
# ESGF installation with Docker Swarm.
# Example script that installs the ESGF software stack 
# on a set of local VMs running Docker Engine in Swarm mode.

# The swarm is composed of:
# - 1 'swarm-manager' manager node
# - 1 'swarm-worker1' worker node, configured to run a customized OODT file manager
# - 2 'swarm-worker2,3' worker nodes, configured to run a customized OODT workflow manager

# create all VMs
docker-machine create -d virtualbox swarm-manager
docker-machine create -d virtualbox swarm-db-worker
docker-machine create -d virtualbox swarm-idp-worker
docker-machine create -d virtualbox swarm-index-worker
docker-machine create -d virtualbox swarm-front-end-worker
docker-machine create -d virtualbox --virtualbox-memory 2048 swarm-data-node-worker

# start the swarm
eval $(docker-machine env swarm-manager)
export MANAGER_IP=`docker-machine ip swarm-manager`
docker swarm init --advertise-addr $MANAGER_IP
token_worker=`docker swarm join-token --quiet worker`
token_manager=`docker swarm join-token --quiet manager`

# drain the swarm manager to prevent assigment of tasks
docker node update --availability drain swarm-manager

# start swarm visualizer on swarm manager
docker run -it -d -p 5000:5000 -e HOST=$MANAGER_IP -e PORT=5000 -v /var/run/docker.sock:/var/run/docker.sock manomarks/visualizer

# join the swarm
eval $(docker-machine env swarm-db-worker)
docker swarm join --token $token_worker $MANAGER_IP:2377

eval $(docker-machine env swarm-idp-worker)
docker swarm join --token $token_worker $MANAGER_IP:2377

eval $(docker-machine env swarm-index-worker)
docker swarm join --token $token_worker $MANAGER_IP:2377

eval $(docker-machine env swarm-front-end-worker)
docker swarm join --token $token_worker $MANAGER_IP:2377

eval $(docker-machine env swarm-data-node-worker)
docker swarm join --token $token_worker $MANAGER_IP:2377

# create overlay network
eval $(docker-machine env swarm-manager)
docker network create -d overlay swarm-network

# assign functional labels to nodes
eval $(docker-machine env swarm-manager)
docker node update --label-add esgf_type=db swarm-db-worker
docker node update --label-add esgf_type=idp swarm-idp-worker
docker node update --label-add esgf_type=index swarm-index-worker
docker node update --label-add esgf_type=front-end swarm-front-end-worker
docker node update --label-add esgf_type=data-node swarm-data-node-worker

# start 'postgres' service
docker service create --replicas 1 --name esgf-postgres -p 5432:5432 --network swarm-network  --constraint 'node.labels.esgf_type==db' esgfhub/esgf-postgres

# start 'idp' service
docker service create --replicas 1 --name esgf-idp-node -p 8445:8443 --network swarm-network  \
                      --mount type=bind,source=$ESGF_CONFIG/esg/config/,target=/esg/config/ \
                      --constraint 'node.labels.esgf_type==idp' esgfhub/esgf-idp-node


# start 'httpd' service using cog_dir volume
docker service create --replicas 1 --name esgf-httpd -p 80:80 -p 443:443 --network swarm-network  \
                      --constraint 'node.labels.esgf_type==front-end' esgfhub/esgf-httpd