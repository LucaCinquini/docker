# Docker image that contains the configuration for an OODT example workflow

FROM oodthub/oodt-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# OODT env
ENV OODT_HOME /usr/local/oodt
ENV OODT_SITE_DIR ${OODT_SITE_DIR:-${OODT_HOME}/mysite}

# install custom OODT File Manager configuration
RUN mkdir -p $OODT_SITE_DIR/products/test-workflow
COPY filemgr_config/policy/ $OODT_SITE_DIR/products/test-workflow/policy/

# install custom OODT Workflow Manager configuration
RUN mkdir -p $OODT_SITE_DIR/workflows/test-workflow
COPY wmgr_config/policy/ $OODT_SITE_DIR/workflows/test-workflow/policy/
COPY wmgr_config/pge-configs/ $OODT_SITE_DIR/workflows/test-workflow/pge-configs/

# install custom PGEs
RUN mkdir -p $OODT_SITE_DIR/pges/test-workflow
COPY pges/ $OODT_SITE_DIR/pges/test-workflow/

VOLUME $OODT_SITE_DIR
