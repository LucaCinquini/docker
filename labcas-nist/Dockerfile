# LabCAS-backend image configured to run NIST data processing workflow

FROM edrn/labcas-backend

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# install R
RUN yum -y update; yum clean all
RUN yum -y install epel-release; yum clean all
RUN yum -y update; yum -y install R; yum clean all


# install NIST workflow to $LABCAS_HOME
RUN cd $LABCAS_SRC/labcas-backend && \
       mvn install -Dworkflow=nist

# copy test data
COPY labcas_staging $LABCAS_STAGING
