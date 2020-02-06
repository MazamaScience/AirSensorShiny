library(futile.logger)
library(MazamaCoreUtils)
library(AirSensor)
library(PWFSLSmoke)
library(AirMonitorPlots)
library(worldmet)
library(future)
library(promises)

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

# Source the R and Module Files
R <- list.files(file.path(paste0(getwd(), '/../../R')), full.names = TRUE)
mods <- list.files(file.path(paste0(getwd(), '/../mods')), full.names = TRUE)
lapply(c(R, mods), source)

# Asynchronous Processing Plan
future::plan(future::multiprocess)

AirSensor::setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

# Instantiate Sensor information
INIT_SENSORS <- AirSensor::sensor_load(days = 1)

SENSOR_LABELS <- INIT_SENSORS$meta$monitorID
SENSOR_COMMUNITIES <- unique(INIT_SENSORS$meta$communityRegion)

PAS <- AirSensor::pas_load()

# Version
VERSION <<- "0.9.1 (beta)"

# Enable Bookmarks / state restoration
shiny::enableBookmarking(store = "url")

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

