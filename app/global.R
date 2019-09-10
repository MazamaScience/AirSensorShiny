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
VERSION <<- "0.4.2"

# Enable Bookmarks / state restoration
shiny::enableBookmarking(store = "url")

# Load R functions
R_utils <- list.files('utils', pattern='^shiny_.+\\.R', full.names=TRUE)
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

logger.debug("LOG_DIR = %s", LOG_DIR)

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
TIMEZONE <<- "America/Los_Angeles"

# Define global pas object
PAS <<- AirSensor::pas_load(archival = TRUE) # TODO:  Should we add "archival = TRUE"?

# NOTE: These are intended to be temporary "translations"
PAS$communityRegion[PAS$communityRegion=="SCAH"] <- "Oakland"
PAS$communityRegion[PAS$communityRegion=="SCUV"] <- "West Los Angeles"
PAS$communityRegion[PAS$communityRegion=="SCAN"] <- "Richmond"


# Define global communities
PAS_COMM <<- na.omit(unique(PAS$communityRegion))

main_helpTxt <<-
  shiny::HTML(
  "<small>
  <p>
  On this page, you can view all of the air quality sensors deployed through the
  US EPA funded STAR Grant at South Coast AQMD, entitled “Engage, Educate
  and Empower California Communities on the Use and Applications of
  Low-cost Air Monitoring Sensors”. The drop down menus above allow you to
  view individual participating communities or highlight individual sensors
  within the pre-selected community. You can control the timeframe shown in
  the bar plot by (1) choosing a date and (2) choosing a number of days to
  “look back”.
  </p>
  <p>
  The colors on the map illustrate the most recent hourly average PM2.5 value,
  for each site. The bar plot (below the map), shows the hourly average PM2.5
  values throughout the selected timeframe, for the selected site/sensor. The
  calendar plot (to the right of the map) shows the historic daily averages for
  the selected site/sensor. Note, the same color scale (in the bottom right)
  defines the colors in the map, bar chart, and calendar plot.
  </p>
  </small>"
  )

comparison_helpTxt <<-
  shiny::HTML(
  "<small>
  <p>
  Once you select a sensor, the map will display the location of the nearest regulatory
  monitoring station (or AirNow site), and the plots will provide a comparison of the data
  from that sensor and the nearest regulatory monitoring site. The time series provides a
  qualitative comparison of the hourly-averaged data, while the scatter plot provides some
  additional statistics to better assess this comparison. In terms of the data displayed in the
  scatter plot – on the horizontal axis (or x-axis) the data from the low-cost air quality sensor
  is plotted, while on the vertical axis (or y-axis) the data from the regulatory monitoring
  station (or AirNow site) is plotted.
  </p>
  <p>
  In terms of the statistics shared in the scatterplot, a slope close to 1.0 indicates that the
  low-cost sensor and the reference reflect similar levels. A slope greater than 1.0 indicates
  higher values are seen at the regulatory monitoring site and vice versa for a slope of less
  than 1.0. The intercept can be an indicator of bias, particularly if the slope is close to 1.0,
  but the intercept is either greater or less than zero. Finally, R2 provides an indication of how
  well the trends agree between the two sites; an R2 closer to 1.0 indicates more agreement
  and an R2 closer to 0.0 indicates less agreement.
  </p>
  </small>"
  )

dailyPatterns_helpTxt <<-
  shiny::HTML(
  "<small>
  <p>
  This tab illustrates the average daily trends for a single sensor using
  two different plots. The bar plot (on the left) lays out the average PM2.5
  values for each hour of the day. The “donut” plot (on the right),
  displays these averages in a circular pattern. Note, for both plots the
  grey shading indicates nighttime hours and the data used to calculate
  these averages is based on the time frame selected.
  </p>
  </small>"
  )

raw_helpTxt <<-
  shiny::HTML(
  "<small>
  <p>
  Here you can view the “raw data” for the selected sensor, during the
  time frame selected. This data has undergone minimal QA/QC allowing
  you to view the highest time resolution PM2.5 data from Channel A,
  Channel B, and the temperature and humidity signals. Also included on
  this page are some additional meteorological data for the time period
  of interest and a pollution rose. The meteorological information can
  help you understand whether any events occurred that might impact
  the air quality or sensor performance. The pollution rose illustrates the
  direction the wind was blowing from when various pollutant
  concentrations were recorded by the selected sensor.
  </p>
  </small>"
  )

animation_helpTxt <<-
  shiny::HTML(
  "<small>
  <p>
  On this page you can view animations of hourly average PM2.5 mass
  concentrations changing over time. Choose your community and a time
  frame and press ‘play’. Using the same color scale as other visualization
  in the app, purple corresponds to higher levels of PM2.5 and light blue
  to lower levels. The shading of the time slider indicates day vs. night,
  with white indicating daytime hours and grey indicating nighttime
  hours.
  </p>
  </small>"
  )
