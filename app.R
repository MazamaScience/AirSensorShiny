#' Start Application

appFolder = 'inst/app'

R_files <- list.files('R', pattern='^shiny_.+\\.R', full.names=TRUE)
module_files <- list.files('inst/mods', full.names = TRUE)
AirSensor::setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
# Load R functions
lapply(R_files,  source)
# Load Modules
lapply(module_files, source)

shiny::shinyAppDir(appFolder)
