# CoG image configured with sqllite backend
# All setup and configuration is executed by the docker entrypoint
# when the container starts up.

FROM esgfhub/cog:v3.6.1

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# default user
USER cogadmin

# default working directory
WORKDIR $COG_INSTALL_DIR

# start up django+CoG with correct host IP
COPY ./docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]

# default host IP
CMD ["localhost"]
