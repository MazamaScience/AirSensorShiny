#' @export
#'
#' @title Leaflet interactive map for use with AirSensorShiny
#'
#' @description This function creates interactive maps that will be displayed in
#' AirSensorShiny web-app.
#'
#' @details The \code{maptype} argument is mapped onto leaflet "ProviderTile"
#' names. Current mappings include:
#' \enumerate{
#' \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"}
#' \item{"terrain"}{ -- "Esri.WorldTopoMap"}
#' \item{"toner"}{ -- "Stamen.Toner"}
#' }
#'
#' If a character string not listed above is provided, it will be used as the
#' underlying map tile if available. See
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
#' "provider tiles" to use as the background map.
#'
#' @param sesnor an airsensor object of PurpleAir synoptic data.
#' @param parameter Value to plot, e.g. \code{pm25_1hr}.
#' @param paletteName A predefined color palette name.
#' @param radius Radius (pixels) of monitor circles.
#' @param opacity Opacity of monitor circles.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.
#' @param pat Optional pat for date range extensions of marker colors.
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'

shiny_sensorLeaflet <- function(
  sensor = NULL,
  startdate = NULL,
  enddate = NULL,
  colorPalette = NULL,
  colorBins = NULL,
  radius = 8,
  opacity = 0.8,
  maptype = "terrain",
  pat = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(sensor)

  if ( !AirSensor::sensor_isSensor(sensor) ) {
    stop("Parameter 'sensor' is not a sensor object.")
  }

  if ( AirSensor::sensor_isEmpty(sensor) ) {
    stop("Parameter 'sensor' contains no data.")
  }

  # ----- Get map parameters ---------------------------------------------------

  lonRange <- range(sensor$meta$longitude, na.rm = TRUE)
  latRange <- range(sensor$meta$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange), diff(latRange), na.rm = TRUE)

  # Determine appropriate zoom level
  if (maxRange > 20) {
    zoom <- 4
  } else if (maxRange > 10) {
    zoom <- 5
  } else if (maxRange > 5) {
    zoom <- 6
  } else if (maxRange > 2) {
    zoom <- 7
  } else if (maxRange > 1) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else {
    zoom <- 12
  }

  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == 'terrain') {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }

  # ----- Generate colors ------------------------------------------------------


  # Calcualte average value per sensor
  mean_pm25 <-
    sensor %>%
    sensor_filterDate(startdate, enddate) %>%
    sensor_extractData() %>%
    dplyr::select(-datetime) %>%
    colMeans(na.rm = TRUE)


  # Default bins
  if ( is.null(colorBins) ) {
    colorBins <- c(0,12,35,55,75,1000)
    # -OR- a gradient as in AirSensor::sensor_createVideoFrame.R
    # colorBins <- c(0,
    #                seq(0,12,length.out=5)[-1],
    #                seq(12,35,length.out=5)[-1],
    #                seq(35,55,length.out=5)[-1],
    #                seq(55,75,length.out=5)[-1],
    #                100,200,500,1000)
  }

  # Default colors
  if ( is.null(colorPalette) ) {
    scaqmd_colors <- c("#abe3f4", "#118cba", "#286096", "#8659a5", "#6a367a")
    colorPalette <- grDevices::colorRampPalette(scaqmd_colors)(length(colorBins)-1)
  }

  # Generate color function
  colorFunc <- leaflet::colorBin(palette = colorPalette,
                                 domain = range(colorBins),
                                 bins = colorBins)

  # Assign colors
  cols <- colorFunc(mean_pm25)

  # ----- Create the SPDF ------------------------------------------------------

  SPDF <-
    sp::SpatialPointsDataFrame(
      data = as.data.frame(sensor$meta),
      coords = cbind(
        sensor$meta$longitude,
        sensor$meta$latitude
      )
    )

  # ----- Create the map -------------------------------------------------------

  map <-
    leaflet::leaflet(SPDF, padding = 0, ) %>%
    leaflet::setView(
      lng=mean(lonRange),
      lat=mean(latRange),
      zoom=zoom
    ) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      group = ~id2com(communityRegion),
      radius=radius,
      fillColor=cols,
      fillOpacity=opacity,
      color = "#FFF",
      opacity = 1,
      weight = 2,
      label = ~monitorID,
      layerId = ~monitorID
    )

  # ----- Return ---------------------------------------------------------------

  return(map)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

  enddate <- MazamaCoreUtils::parseDatetime("2019-07-06",
                                            timezone = "America/Los_Angeles")
  startdate <- enddate - lubridate::ddays(3)
  sensor = sensor_load(startdate = startdate, enddate = enddate)
  colorPalette <- NULL
  colorBins <- NULL
  radius <- 9
  opacity <- 0.8
  maptype <- "terrain"

  map <- shiny_sensorLeaflet(
    sensor = sensor,
    startdate = startdate,
    enddate = enddate,
    maptype = "OpenStreetMap",
    radius = 6,
    opacity = 0.95,
  )

  print(map)

}
