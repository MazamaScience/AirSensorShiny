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
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'

shiny_sensorLeaflet <- function(
  sensor = NULL,
  startdate = NULL,
  enddate = NULL,
  period = "3 day",
  radius = 9,
  opacity = 0.8,
  maptype = "terrain"
) {

  # TODO: ADD Checks and test for sensor object

  # Extract view information
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

  # sensor <- shiny_sensorMeanAggregate(sensor, period)
  #
  # sensor$meta$x_mean <- t(sensor$data[nrow(sensor$data)-1,][c(-1)])

  # sensor <- PWFSLSmoke::monitor_subset(ws_monitor = sensor, tlim = c(startdate, enddate))

  # colBreaks <- purrr::map(sensor$data[c(-1)], cut, breaks = c(0,12,35,55,75,6000))
#
#   binpal <- leaflet::colorNumeric(palette = c(
#     ">75 \u03bcg / m\u00b3" = "#6A367A",
#     "55-75 \u03bcg / m\u00b3" = "#8659A5",
#     "35-55 \u03bcg / m\u00b3" = "#286096",
#     "12-35 \u03bcg / m\u00b3" ="#118CBA",
#     "0-12 \u03bcg / m\u00b3" = "#abe3f4"
#   ), sensor$meta$x_mean)

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

  SPDF <-
    sp::SpatialPointsDataFrame(
      data = as.data.frame(sensor$meta),
      coords = cbind(
        sensor$meta$longitude,
        sensor$meta$latitude
      )
    )

  # Create leaflet map
  map <-
    leaflet::leaflet(SPDF) %>%
    leaflet::setView(
      lng=mean(lonRange),
      lat=mean(latRange),
      zoom=zoom
    ) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      radius=5,#radius,
      #fillColor=cols,
      fillOpacity=1,
      stroke=TRUE,
      color = ~binpal(x_mean),#"#77A4B2",
      weight = "1",
      label = sensor$meta$label,
      layerId = sensor$meta$label
    )

  return(map)

}
