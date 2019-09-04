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

# Version
VERSION <<- 0.4

# Enable Bookmarks / state restoration
shiny::enableBookmarking(store = "url")

# Load R functions
R_utils <- list.files('utils/', pattern='^shiny_.+\\.R', full.names=TRUE)
for ( file in R_utils ) {
  source( file.path(getwd(),file) )
}


# ----- Set up logging ---------------------------------------------------------

if ( interactive() ) { # Running from RStudio

  # Somewhere easy to find
  LOG_DIR <- file.path(getwd(),"../logs")

} else {

  # Use the shiny-server default
  LOG_DIR <- "/var/log/shiny-server/"

}

MazamaCoreUtils::initializeLogging(LOG_DIR)

if ( interactive() ) { # Running from RStudio
  logger.setLevel(TRACE)
}

# Log session info
logger.debug(capture.output(sessionInfo()))

# ----- Global settings --------------------------------------------------------

# Set the archive base url
AirSensor::setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

# Helpful conversion list
CommunityById <- list(
  "SCAP" = "Alhambra/Monterey Park",
  "SCBB" = "Big Bear Lake",
  "SCEM" = "El Monte",
  "SCIV" = "Imperial Valley",
  "SCNP" = "Nipomo",
  "SCPR" = "Paso Robles",
  "SCSJ" = "San Jacinto",
  "SCSB" = "Seal Beach",
  "SCAH" = "Oakland",
  "SCAN" = "Richmond",
  "SCUV" = "West Los Angeles",
  "SCSG" = "South Gate",
  "SCHS" = "Sycamore Canyon",
  "SCTV" = "Temescal Valley"
)

# Helpful conversion list
IdByCommunity <- list(
  "Alhambra/Monterey Park" = "SCAP",
  "Big Bear Lake" = "SCBB",
  "El Monte" = "SCEM",
  "Imperial Valley" = "SCIV",
  "Nipomo" = "SCNP",
  "Paso Robles" = "SCPR",
  "San Jacinto" = "SCSJ",
  "Seal Beach" = "SCSB",
  "Richmond" = "SCAH",
  "Oakland" = "SCAN",
  "West Los Angeles " = "SCUV",
  "South Gate" = "SCSG",
  "Sycamore Canyon" = "SCHS",
  "Temescal Valley" = "SCTV"
)

# Set Timezone
TIMEZONE <- "America/Los_Angeles"

# Define global pas object
PAS <- AirSensor::pas_load(archival = TRUE) # TODO:  Should we add "archival = TRUE"?

# NOTE: These are intended to be temporary "translations"
PAS$communityRegion[PAS$communityRegion=="SCAH"] <- "Oakland"
PAS$communityRegion[PAS$communityRegion=="SCUV"] <- "West Los Angeles"
PAS$communityRegion[PAS$communityRegion=="SCAN"] <- "Richmond"


# Define global communities
PAS_COMM <- na.omit(unique(PAS$communityRegion))
