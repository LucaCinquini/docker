# CoG image configured with sqllite backend
FROM lucacinquini/cog

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# configure and upgrade CoG without ESGF
RUN cd $COG_INSTALL_DIR && \
    source $COG_DIR/venv/bin/activate && \
    python setup.py -q setup_cog --esgf=false

# PRODUCTION_SERVER = True would require use of SSL to transmit any cookie
RUN sed -i 's/PRODUCTION_SERVER = True/PRODUCTION_SERVER = False/g' $COG_CONFIG_DIR/cog_settings.cfg
