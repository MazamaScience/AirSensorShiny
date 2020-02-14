################################################################################
# Top level application wrapper

# Folder with top level application code
appFolder = 'inst/app'

# Load R functions
R_files <- list.files('R', pattern = '^shiny_.+\\.R', full.names = TRUE)
lapply(R_files,  source)

# Load Shiny UI modules
module_files <- list.files('inst/mods', full.names = TRUE)
lapply(module_files, source)

# Set data archive URL
AirSensor::setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

# Run the shiny app
shiny::shinyAppDir(
  ui = appFolder,
  options = list(
    # NOTE:  Options and defaults from ?shiny::runApp
    # port = getOption("shiny.port"),
    # launch.browser = getOption("shiny.launch.browser", interactive()),
    # host = getOption("shiny.host", "127.0.0.1"),
    # quiet = FALSE,
    # display.mode = c("auto", "normal", "showcase"),
    # test.mode = getOption("shiny.testmode", FALSE)
  )
)
