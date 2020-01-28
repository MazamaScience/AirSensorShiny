library(futile.logger)
library(MazamaCoreUtils)
library(AirSensor)
library(PWFSLSmoke)
library(AirMonitorPlots)
library(worldmet)
library(future)
library(promises)

R <- list.files(file.path(paste0(getwd(), '/../../R')), full.names = TRUE)
mods <- list.files(file.path(paste0(getwd(), '/../mods')), full.names = TRUE)
lapply(c(R, mods), source)

# Asynchronous Processing Plan
future::plan(future::multiprocess)

AirSensor::setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

# Instantiate Sensor information
INIT_SENSORS <- AirSensor::sensor_load(days =1)
SENSOR_LABELS <- INIT_SENSORS$meta$monitorID
SENSOR_COMMUNITIES <- unique(INIT_SENSORS$meta$communityRegion)

# Version
VERSION <<- "0.9c (beta)"

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

