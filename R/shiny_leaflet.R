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
#' @param pas Enhanced dataframe of PurpleAir synoptic data.
#' @param parameter Value to plot, e.g. \code{pm25_1hr}.
#' @param paletteName A predefined color palette name.
#' @param radius Radius (pixels) of monitor circles.
#' @param opacity Opacity of monitor circles.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'

shiny_leaflet <- function(
  pas = NULL,
  parameter = "pm25_1hr",
  paletteName = NULL,
  radius = 9,
  opacity = 0.8,
  maptype = "terrain"
) {

  logger.debug('----- shiny_leaflet() -----')

  # ----- Validate parameters --------------------------------------------------

  if ( !("data.frame" %in% class(pas)) )
    stop(paste0("First argument is not of class 'data.frame'."))

  if ( nrow(pas) == 0 || ncol(pas) == 0 )
    stop(paste0("One or both dimensions of the pa_synoptic object has length 0."))

  if ( !is.numeric(radius) )
    stop(paste0('radius parameter is non-numeric'))

  colorInfo <- AirSensor::pas_palette(
    pas,
    paletteName =
      c(
        "purple2" = "#6b0096",
        "purple1" = "#9f00de",
        "blue3" = "#002ade",
        "blue2" = "#3b8aff",
        "blue1" = "#abebff"
      ),
    parameter = parameter,
    reverse= TRUE
  )

  cols <- colorInfo$colors

  # Extract view information
  lonRange <- range(pas$longitude, na.rm = TRUE)
  latRange <- range(pas$latitude, na.rm = TRUE)
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

  # Convert locations to SpatialPointsDataFrame
  pas <- pas[!is.na(pas$latitude),]

  pas <- pas[which(!stringr::str_detect(pas$label, " B")),]
  SPDF <-
    sp::SpatialPointsDataFrame(
      data = as.data.frame(pas),
      coords = cbind(
        pas$longitude,
        pas$latitude
      )
    )

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

  # Create leaflet map
  map <-
    leaflet::leaflet(SPDF,padding =100)
  map <-
    leaflet::setView(
      map,
      lng=mean(lonRange),
      lat=mean(latRange),
      zoom=zoom
    )
  map <-
    leaflet::addProviderTiles(map, providerTiles)

  map <-
    leaflet::addCircleMarkers(
      map,
      radius=radius,
      fillColor=cols,
      fillOpacity=opacity,
      color = "#000",
      weight = "1",
      label = ~label,
      layerId = pas$label
    )

  return(map)

}
