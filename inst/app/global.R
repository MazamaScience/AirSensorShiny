# Load R functions
lapply(list.files('R', pattern='^shiny_.+\\.R', full.names=TRUE),  source)
# Load Modules
lapply(list.files('inst/mods', full.names = TRUE), source)

# Instantiate Sensor information
INIT_SENSORS <- sensor_load(days = 1)
SENSOR_LABELS <- INIT_SENSORS$meta$monitorID
SENSOR_COMMUNITIES <- unique(INIT_SENSORS$meta$communityRegion)
