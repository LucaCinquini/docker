FROM esgfhub/esgf-node

MAINTAINER Luca Cinquini <luca.cinquini@jpl.nasa.gov>

RUN yum install -y httpd

EXPOSE 80 443

ENTRYPOINT ["apachectl", "-D", "FOREGROUND"]
