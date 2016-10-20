#! /bin/bash
# driver script to initialize an ESGF node
# Usage:    esgf_node_init.sh -h <hostname> -d <configuration directory>
# Example:  esgf_node_init.sh -h <my-node.esgf.org> -d ~/ESGF_CONFIG
#
# This script will:
# o initialize the content of a new site-specific configuration directory: <configuration directory>
# o generate new self-signed certificates for the given <hostname>
# o change all site configuration to use <hostname>

# argument default values
hostname=''
configdir=''

# parse command line arguments
while getopts 'h:p:d:' flag; do
  case "${flag}" in
    h) hostname="${OPTARG}" ;;
    d) configdir="${OPTARG}" ;;
  esac
done
if [ "$hostname" = "" ] || [ "$configdir" = "" ];
then
   echo "Usage: esgf_node_init.sh -h <hostname> -d <configuration directory>" 
   exit -1
else
   echo "Using hostname=$hostname"
   echo "Using configdir=$configdir"
fi

# set env variables needed by child scripts
export ESGF_CONFIG=$configdir
export ESGF_HOSTNAME=$hostname

# initialize the node configuration directory
echo ""
echo "Initializing the node configuration directory with default content..."
echo "Removing any existing content..."
if [ -e $configdir ]
then
   rm -rf $configdir/*
else
   mkdir -p $configdir
fi
cp -R ../esgf_config/* $configdir/.

# generate new certificates
echo ""
echo "Generating new self-signed certificates..."
./generate_certificates.sh $hostname

# change node configuration to use new hostname
echo ""
echo "Changing configuration for hostname=$hostname..."
./change_hostname.sh $hostname

echo "... ESGF node initialization completed."
