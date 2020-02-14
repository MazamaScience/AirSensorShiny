#' Global
#' Used to define global variables and loading
#'

# Load the libraries
suppressPackageStartupMessages({
  library(futile.logger) # logging
  library(MazamaCoreUtils) # Core
  library(AirSensor) # AirSensor duh
  library(PWFSLSmoke) # Monitors
  library(AirMonitorPlots) # Plotting Extension
  library(ggplot2)
  library(plotly)
  library(worldmet) # Wind Info
  library(future) # Async
  library(promises) # Async
})

# ----- Configurable options ---------------------------------------------------
 
# NOTE: Update this via the Makefile -- configure_app will keep all versions
#       across docker, make, and app the same.
VERSION <<- "0.9.4"

TZ <- "America/Los_Angeles"

# ----- Utility functions ------------------------------------------------------

rm_invalid <- function(x) {
  sensor_filterMeta(x, !is.na(communityRegion))
}

# Helpful conversion list
com_id  <-
  data.frame(
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

# 4 Digit code to Community name
id2com <- function(X) {
  unlist(lapply(X, function(x) {ifelse(is.null(com_id[[x]]), x, levels(com_id[[x]]))}))
}

# Community name to 4 digit code
com2id <- function(X) {
  unlist(lapply(X, function(x) {i<-which(com_id == x); ifelse(length(i)!=0, names(com_id[i]), x)}))
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

logger.debug("VERSION = %s", VERSION)
logger.debug("TZ = %s", TZ)
logger.debug("LOG_DIR = %s", LOG_DIR)

# ----- More setup -------------------------------------------------------------

# Asynchronous Processing Plan
future::plan(future::multiprocess)

# Load PurpleAir Synoptic data
PAS <- AirSensor::pas_load()

# Instantiate Sensor information
INIT_SENSORS <- rm_invalid(AirSensor::sensor_load(days = 5))

SENSOR_LABELS <- INIT_SENSORS$meta$monitorID
SENSOR_COMMUNITIES <- unique(INIT_SENSORS$meta$communityRegion)

# Enable Bookmarks / state restoration
shiny::enableBookmarking(store = "url")


