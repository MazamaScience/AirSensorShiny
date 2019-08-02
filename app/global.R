#
# This is for defining global variables and imports for the AirShiny web
# application. You can run the application by clicking 'Run App' above.
#
# Find out more about Shiny applications here:
#
#    http://shiny.rstudio.com/
#
# - Mazama Science
#

library(AirSensor)
library(MazamaCoreUtils)

# Define global pas object
PAS <- AirSensor::pas_load()

# Define global communities
PAS_COMM <- na.omit(unique(PAS$communityRegion))

# Source the internal Utils for shiny
source(paste0(getwd(),"/utils.R"))
