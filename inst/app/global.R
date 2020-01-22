library(AirSensor)
library(PWFSLSmoke)
AirSensor::setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
# Load R functions
lapply(list.files('R', pattern='^shiny_.+\\.R', full.names=TRUE),  source)
# Load Modules
lapply(list.files('inst/mods', full.names = TRUE), source)

# Instantiate Sensor information
INIT_SENSORS <- AirSensor::sensor_load(days =1)
SENSOR_LABELS <- INIT_SENSORS$meta$monitorID
SENSOR_COMMUNITIES <- unique(INIT_SENSORS$meta$communityRegion)

#' Start Application
#'
#' @param port port of web application
#'
#' @export
startApplication <- function(port = 4242, appFolder = "app") {
  runApp(
    system.file(appFolder, package = "myPackageName"),
    port = port,
    host = "0.0.0.0"
  )
}

