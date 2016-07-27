FROM esgfhub/cog:devel

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# default working directory
WORKDIR $COG_INSTALL_DIR

# initialize postgres connection parameters
COPY cog_settings.cfg $COG_CONFIG_DIR/cog_settings.cfg

# start up django+CoG with correct host IP
COPY ./docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]

# default host IP
CMD ["localhost"]
