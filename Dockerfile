#
# Rock R Server Dockerfile with DataSHIELD
#
# https://github.com/obiba/docker-rock-demo
#

FROM datashield/rock-base:6.2-R4.2

ENV DSYNTHETIC_VERSION 0.0.2

ENV ROCK_LIB /var/lib/rock/R/library

# Additional system dependencies
#RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y ???

# Update R packages
#RUN Rscript -e "update.packages(ask = FALSE, repos = c('https://cloud.r-project.org'), instlib = '/usr/local/lib/R/site-library')"

# Install new R packages

# dsSynthetic
RUN Rscript -e "remotes::install_github('tombisho/dsSynthetic', ref = '$DSYNTHETIC_VERSION', dependencies = TRUE, upgrade = FALSE, lib = '$ROCK_LIB')"

RUN chown -R rock $ROCK_LIB
