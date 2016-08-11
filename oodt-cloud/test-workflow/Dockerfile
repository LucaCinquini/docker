# Docker image that contains the configuration for an OODT example workflow

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# OODT env
ENV OODT_HOME /usr/local/oodt

# install custom OODT File Manager configuration
RUN mkdir -p $OODT_HOME/config/products/test-product
COPY filemgr_config/policy/ $OODT_HOME/config/products/test-product/policy/

# install custom OODT Workflow Manager configuration
RUN mkdir -p $OODT_HOME/config/workflows/test-workflow
COPY wmgr_config/policy/ $OODT_HOME/config/workflows/test-workflow/policy/
COPY wmgr_config/pge-configs/ $OODT_HOME/config/workflows/test-workflow/pge-configs/

VOLUME $OODT_HOME/config
