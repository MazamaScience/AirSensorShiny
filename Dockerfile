FROM mazamascience/airsensor:0.3.2

LABEL maintainer="hans@mazamascience.com" \
      maintainer="jon@mazamascience.com"
 
LABEL build_date="2019-06-26"
LABEL version="1.0"

LABEL description="AirShiny Docker Image"

# Copy app directory to image 
COPY app /srv/shiny-server/

# Allow readable files
RUN chmod -R 755 /srv/shiny-server/

# Port 
EXPOSE 3838

# Run 
CMD ["/usr/bin/shiny-server.sh"] 
