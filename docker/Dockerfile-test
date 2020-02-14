FROM mazamascience/airsensordataviewer:0.9.4

LABEL maintainer="hans@mazamascience.com" \
      maintainer="jon@mazamascience.com"

LABEL description="AirSensor DataViewer Docker Image"

################################################################################
# install software / cleanup

# Remove sample apps
RUN rm -rf /srv/shiny-server/

################################################################################
# app
#
# Setup for MS Azure

# Instance configuration ---------------

# Port
EXPOSE 3838

# Set up the app ---------------

# Copy app
COPY inst /srv/shiny-server/inst
COPY R /srv/shiny-server/R
COPY app.R /srv/shiny-server/app.R
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Allow readable files
RUN chmod -R 755 /srv/shiny-server/inst/app

# Run
CMD ["/usr/bin/shiny-server.sh"]
