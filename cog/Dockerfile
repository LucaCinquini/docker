# Basic CoG image containing CoG software and all required dependencies.
# No configuration, cannot be run standalone.

FROM lucacinquini/centos67_python27:latest

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# choose CoG version
ENV COG_TAG=v3.6.0

# setup CoG environment
ENV COG_DIR=/usr/local/cog
RUN mkdir -p $COG_DIR
ENV COG_CONFIG_DIR=$COG_DIR/cog_config
RUN mkdir -p $COG_CONFIG_DIR
ENV COG_INSTALL_DIR=$COG_DIR/cog_install
RUN mkdir -p $COG_INSTALL_DIR
ENV LD_LIBRARY_PATH=/usr/local/lib

# install CoG in virtual environment
RUN cd $COG_DIR && \
    virtualenv venv && \
    source $COG_DIR/venv/bin/activate && \
    git clone https://github.com/EarthSystemCoG/COG cog_install

# checkout specific tag
RUN cd $COG_INSTALL_DIR && \
    git checkout $COG_TAG

# install CoG dependencies
RUN cd $COG_INSTALL_DIR && \
    source $COG_DIR/venv/bin/activate && \
    python setup.py -q install

# manually install additional dependencies
RUN cd $COG_DIR && \
    source $COG_DIR/venv/bin/activate && \
    git clone https://github.com/EarthSystemCoG/django-openid-auth.git && \
    cd django-openid-auth && \
    python setup.py install

RUN cd $COG_DIR && \
    git clone https://github.com/globusonline/transfer-api-client-python.git && \
    cd transfer-api-client-python && \
    source $COG_DIR/venv/bin/activate && \
    python setup.py install && \
    git pull && \
    cd mkproxy && \
    make  && \
    cp mkproxy $COG_DIR/venv/lib/python2.7/site-packages/globusonline_transfer_api_client-0.10.16-py2.7.egg/globusonline/transfer/api_client/x509_proxy/.
