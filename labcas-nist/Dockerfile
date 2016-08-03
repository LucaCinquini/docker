# LabCAS-backend image configured to run NIST data processing workflow

FROM edrn/labcas-backend

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

# install R
RUN yum -y update; yum clean all
RUN yum -y install epel-release; yum clean all
RUN yum -y update; yum -y install R; yum clean all

# install R dependencies
COPY packages.R /tmp/packages.R
RUN cd /tmp && \
    R CMD BATCH packages.R

# install NIST workflow to $LABCAS_HOME
RUN cd $LABCAS_SRC/labcas-backend && \
    git pull && \
    mvn install -Dworkflow=nist

# copy test data
COPY labcas_staging $LABCAS_STAGING 
RUN chown -R labadmin:labadmin $LABCAS_STAGING 

# copy ancillary data
COPY pge_root $PGE_ROOT
RUN chown -R labadmin:labadmin $PGE_ROOT

# FIXME
RUN chown -R labadmin:labadmin $LABCAS_HOME/workflows/nist/pges
