# Docker image based on Centos 6.7 
# with additional Python 2.7 installation and a few common Python packages

FROM centos:centos6.7

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

ENV PYTHON_VERSION 2.7.9
ENV PILLOW_VERSION 2.8.2

# install yum dependencies
RUN yum -y update && \
    yum groupinstall -y development && \
    yum install -y \
    yum-utils \
    bzip2-devel \
    git \
    which \
    hostname \
    openssl \
    openssl-devel \
    sqlite-devel \
    sudo \
    tar \
    wget \
    zlib-dev \
    sqlite-devel \
    freetype-devel \
    postgresql-devel \
    libjpeg-turbo-devel

# install python2.7
RUN cd /tmp && \
    wget https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tgz && \
    tar xvfz Python-${PYTHON_VERSION}.tgz && \
    cd Python-${PYTHON_VERSION} && \
    ./configure --prefix=/usr/local && \
    make && \
    make altinstall
RUN ln -s /usr/local/bin/python2.7 /usr/local/bin/python
ENV PATH=/usr/local/bin:$PATH

# install additional core Python packages
RUN cd /tmp
RUN wget --no-check-certificate https://bootstrap.pypa.io/ez_setup.py
RUN python ez_setup.py --insecure
RUN rm -rf ez_setup.py
RUN easy_install pip
RUN pip install virtualenv
RUN pip install wheel
RUN pip install --use-wheel Pillow==${PILLOW_VERSION}
