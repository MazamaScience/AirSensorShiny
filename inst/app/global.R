library(futile.logger)
library(MazamaCoreUtils)
library(AirSensor)
library(PWFSLSmoke)
library(AirMonitorPlots)
library(worldmet)

#' Start Application
#'
#' @param port port of web application
#'
#' @export
startApplication <- function(port = 4242, appFolder = 'inst/app') {
  shiny::runApp(appFolder, port, launch.browser = TRUE)
}

AirSensor::setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
# Load R functions
lapply(list.files('R', pattern='^shiny_.+\\.R', full.names=TRUE),  source)
# Load Modules
lapply(list.files('inst/mods', full.names = TRUE), source)

# Instantiate Sensor information
INIT_SENSORS <- AirSensor::sensor_load(days =1)
SENSOR_LABELS <- INIT_SENSORS$meta$monitorID
SENSOR_COMMUNITIES <- unique(INIT_SENSORS$meta$communityRegion)

# Version
VERSION <<- "0.7"

# Enable Bookmarks / state restoration
shiny::enableBookmarking(store = "url")

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

logger.debug("LOG_DIR = %s", LOG_DIR)

