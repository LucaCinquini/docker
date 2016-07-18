# CoG image configured with sqllite backend
# All setup and configuration is executed by the docker entrypoint
# when the container starts up.

FROM lucacinquini/cog

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# start up django+CoG with correct host IP
COPY ./docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]

# default host IP
CMD ["localhost"]
