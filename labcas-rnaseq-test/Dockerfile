# LabCAS-backend image configured with fake RNA sequence PGEs

FROM edrn/labcas-backend

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

USER root

# update LabCAS installation
RUN cd $LABCAS_SRC/labcas-backend && \
    git checkout -b devel origin/devel && \
    git pull

# update common LabCAS libraries
RUN cd $LABCAS_SRC/labcas-backend && \
       mvn install

# install LabCAS rnaseq workflow
RUN cd $LABCAS_SRC/labcas-backend && \
       mvn install -Dworkflow=rnaseq

ENV RNASEQ_INSTALL_DIR $PGE_ROOT/rnaseq
RUN mkdir -p $RNASEQ_INSTALL_DIR

# install test PGEs
RUN yum install -y python-argparse

RUN mkdir -p $RNASEQ_INSTALL_DIR/tophat 
RUN cp $LABCAS_SRC/labcas-backend/workflows/src/main/resources/rnaseq/pges-test/tophat $RNASEQ_INSTALL_DIR/tophat/tophat
ENV PATH $PATH:$RNASEQ_INSTALL_DIR/tophat

RUN mkdir -p $RNASEQ_INSTALL_DIR/samtools_install/bin && \
    cp $LABCAS_SRC/labcas-backend/workflows/src/main/resources/rnaseq/pges-test/samtools $RNASEQ_INSTALL_DIR/samtools_install/bin/.
ENV PATH $PATH:$RNASEQ_INSTALL_DIR/samtools_install/bin

RUN cp $LABCAS_SRC/labcas-backend/workflows/src/main/resources/rnaseq/pges-test/htseq-count $RNASEQ_INSTALL_DIR/.
ENV PATH $PATH:$RNASEQ_INSTALL_DIR
