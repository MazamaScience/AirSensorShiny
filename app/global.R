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

# Load R functions
R_files <- list.files('.', pattern='^shiny_.+\\.R', full.names=TRUE)
for ( file in R_files ) {
  source( file.path(getwd(),file) )
}


# ----- Set up logging ---------------------------------------------------------

if ( interactive() ) { # Running from RStudio

  LOG_DIR <- file.path(getwd(),"logs")

} else {

  LOG_DIR <- Sys.getenv("LOG_DIR")

}

dir.create(LOG_DIR, showWarnings = FALSE)

logger.setup(
  traceLog = file.path(LOG_DIR, "AirSensorShiny_TRACE.log"),
  debugLog = file.path(LOG_DIR, "AirSensorShiny_DEBUG.log"),
  infoLog  = file.path(LOG_DIR, "AirSensorShiny_INFO.log"),
  errorLog = file.path(LOG_DIR, "AirSensorShiny_ERROR.log")
)

if ( interactive() ) { # Running from RStudio
  logger.setLevel(TRACE)
}

# Log session info
logger.debug(capture.output(sessionInfo()))

# ----- Global settings --------------------------------------------------------

# Set the archive base url
AirSensor::setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

# Define global pas object
PAS <- AirSensor::pas_load() # TODO:  Should we add "archival = TRUE"?

# Define global communities
PAS_COMM <- na.omit(unique(PAS$communityRegion))

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
  "SCAH" = "SCAH",
  "SCAN" = "SCAN",
  "SCUV" = "SCUV",
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
  "SCAH" = "SCAH",
  "SCAN" = "SCAN",
  "SCUV" = "SCUV",
  "South Gate" = "SCSG",
  "Sycamore Canyon" = "SCHS",
  "Temescal Valley" = "SCTV"
)
