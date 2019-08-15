#'
#' @export
#'
#' @title Leaflet interactive map for use with AirShiny
#'
#' @description This function creates interactive maps that will be displayed in
#' AirShiny web-app.
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

utils_leaflet <- function(
  pas = NULL,
  parameter = "pm25_1hr",
  paletteName = NULL,
  radius = 11,
  opacity = 0.8,
  maptype = "terrain"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( !("data.frame" %in% class(pas)) )
    stop(paste0("First argument is not of class 'data.frame'."))

  if ( nrow(pas) == 0 || ncol(pas) == 0 )
    stop(paste0("One or both dimensions of the pa_synoptic object has length 0."))

  if ( !is.numeric(radius) )
    stop(paste0('radius parameter is non-numeric'))

  colorInfo <- AirSensor::pas_palette(
    pas,
    paletteName = paletteName,
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
    leaflet::leaflet(SPDF)

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
      stroke=FALSE,
      layerId = pas$label
    )

  return(map)

}

#'
#' @export
#' @title Bar plot for AirShiny
#'
#' @param pat PurpleAir Timeseries "pat" object from \code{pat_createNew()}
#' @param period The time period to average to. Can be "sec", "min", "hour",
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param startdate The start date. Used to provide the Datetime domain.
#' @param enddate The end date. Used to provide the Datetime domain.
#' @param ylim Y-axis limits.
#'
#' @description A barplot that bins by period and preforms an average of
#' Channel A & B, or seperately. Intended for use with the AirShiny web app.
#' @details The \code{ylim} is used to fix the Y-axis domain in order to have
#' consistent visualization.The X-axis is determined by the
#' datetime interval.
#'
#'

utils_barplot <-
  function(
    pat,
    period,
    startdate,
    enddate,
    ylim = NULL
  ) {

    # ----- Validate parameters ------------------------------------------------

    if ( AirSensor::pat_isEmpty(pat) )
      stop("Required Purple Air time series is missing")

    if ( !AirSensor::pat_isPat(pat) )
      stop("Required parameter 'pat' is not a valid 'pa_timerseries' object.")

    MazamaCoreUtils::logger.debug(" # utils_barplot() # ")

    ast <-
      AirSensor::pat_createAirSensor(
        pat = pat,
        period = period,
        channel = "ab"
      )

    # Create color palette
    palette <- grDevices::colorRampPalette(colors = rev(c("#9733ee", "#da22ff")))

    # Plot
    pm25_plot <-
      ast$data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = lubridate::interval(
            startdate,
            enddate,
            tzone = "America/Los_Angeles"
          ),
          y = ast$data[[2]],
          colours = ast$data[[2]]
        )
      ) +
      ggplot2::ggtitle(
        label = "PM2.5"
      ) +
      ggplot2::coord_cartesian(ylim = ylim) +
      ggplot2::xlab("Datetime") +
      ggplot2::ylab("\u03bcg / m\u00b3") +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_viridis_c(begin = 0.15) +
      ggplot2::scale_x_datetime(date_breaks = "1 day") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = ifelse(enddate - startdate > 10, 25, 0),
          hjust = 1,
          size = 12),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.title.y = ggplot2::element_text(size = 14)
      )



    # Average PM2.5 barplot
    pm25_avg_bar <-
      ggplot2::geom_bar(
        data = ast$data,
        mapping = ggplot2::aes(
          x = .data$datetime,
          y = ast$data[[2]],
          fill = ast$data[[2]]),
        stat = "identity",
        color = "gray95",
        show.legend = FALSE
      )

    gg <-
      pm25_plot +
      pm25_avg_bar


    return(gg)

  }

comp_leaflet <- function(
  pas = NULL,
  parameter = "pm25_1hr",
  paletteName = NULL,
  radius = 11,
  opacity = 0.8,
  maptype = "terrain"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( !("data.frame" %in% class(pas)) )
    stop(paste0("First argument is not of class 'data.frame'."))

  if ( nrow(pas) == 0 || ncol(pas) == 0 )
    stop(paste0("One or both dimensions of the pa_synoptic object has length 0."))

  if ( !is.numeric(radius) )
    stop(paste0('radius parameter is non-numeric'))

  colorInfo <- AirSensor::pas_palette(
    pas,
    paletteName = paletteName,
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

  #PWFSLSmoke::monitor_load("20190101", "20190701", monitorIDs = pas$pwfsl_closestMonitorID[1])

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
    leaflet::leaflet(SPDF)

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
      stroke=FALSE,
      layerId = pas$label
    )

  return(map)

}
#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#' @import graphics
#'
#' @title Linear model fitting of PurpleAir and federal PWFSL time series data
#'
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param showPlot Logical specifying whether to generate a model fit plot.
#' @param size Size of points.
#' @param pa_color Color of hourly points.
#' @param pwfsl_color Color of hourly points.
#' @param alpha Opacity of points.
#' @param lr_shape Symbol to use for linear model points.
#' @param lr_color Color of linear model plot points.
#' @param lr_lwd Width of linear regression line.
#' @param lr_lcolor Color of linear regression line.
#' @param lr_lalpha Opacity of linear regression line.
#' @param ts_shape Symbol to use for time series points.
#' @param xylim Vector of (lo,hi) limits used as limits on the correlation plot
#' axes -- useful for zooming in.
#' @param channel Data channel to use for PM2.5 -- one of "a", "b or "ab".
#' @param replaceOutliers Logical specifying whether or not to replace outliers.
#' @param qc_algorithm Named QC algorithm to apply to hourly aggregation stats.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#'
#' @description Produces a linear model between data from PurpleAir and data
#' from the closest PWFSL monitor.
#'
#' A diagnostic plot is produced if `showPlot = TRUE`.
#'
#' @return A linear model, fitting the `pat` PurpleAir readings to the closest
#' PWFSL monitor readings.
#'


utils_externalFit <- function(
  pat = NULL,
  showPlot = TRUE,
  size = 1,
  pa_color = "purple",
  pwfsl_color = "black",
  alpha = 0.5,
  lr_shape = 15,
  lr_color = "black",
  lr_lwd = 1.5,
  lr_lcolor = "tomato",
  lr_lalpha = 0.45,
  ts_shape = 1,
  xylim = NULL,
  channel = "ab",
  replaceOutliers = TRUE,
  qc_algorithm = "hourly_AB_01",
  min_count = 20
) {

  # ----- Validate parameters --------------------------------------------------

  if ( !AirSensor::pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")

  if ( AirSensor::pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")

  # For easier access
  meta <- pat$meta
  data <- pat$data

  # ----- Assemble data ---------------------------------------------

  if ( replaceOutliers )
    pat <- AirSensor::pat_outliers(pat, showPlot = FALSE, replace = TRUE)

  # Get the hourly aggregated PurpleAir data
  paHourly_data <-
    pat %>%
    AirSensor::pat_createAirSensor(period = "1 hour",
                        channel = channel,
                        qc_algorithm = qc_algorithm,
                        min_count = min_count) %>%
    PWFSLSmoke::monitor_extractData()
  names(paHourly_data) <- c("datetime", "pa_pm25")

  # Get the PWFSL monitor data
  monitorID <- pat$meta$pwfsl_closestMonitorID
  tlim <- range(paHourly_data$datetime)
  pwfsl_data <-
    PWFSLSmoke::monitor_load(tlim[1], tlim[2], monitorIDs = monitorID) %>%
    PWFSLSmoke::monitor_subset(tlim = tlim) %>%
    PWFSLSmoke::monitor_extractData()
  names(pwfsl_data) <- c("datetime", "pwfsl_pm25")

  # Combine data from both monitors into one dataframe
  both_data <- dplyr::full_join(paHourly_data, pwfsl_data, by = "datetime")

  # Create a tidy dataframe appropriate for ggplot
  tidy_data <-
    both_data %>%
    tidyr::gather("source", "pm25", -.data$datetime)

  # Define square xy limit now that we have the data for both monitors
  if ( is.null(xylim) ) {
    dataMin <- min(c(0, both_data$pa_pm25, both_data$pwfsl_pm25), na.rm = TRUE)
    dataMax <- max(c(both_data$pa_pm25, both_data$pwfsl_pm25), na.rm = TRUE)
    xylim <- c(dataMin, dataMax)
  }

  # ----- Linear model ---------------------------------------------------------

  # Model PWSFL as a function of PurpleAir (data should lie on a line)
  model <- lm(both_data$pwfsl_pm25 ~ both_data$pa_pm25, subset = NULL,
              weights = NULL)

  slope <- as.numeric(model$coefficients[2])      # as.numeric() to remove name
  intercept <- as.numeric(model$coefficients[1])
  r_squared <- summary(model)$r.squared

  # Label for linear fit
  equationLabel <-
    ggplot2::annotate(
      geom = "text",
      x = 0.75 * xylim[2],
      y = c(0.25, 0.15, 0.05) * xylim[2],
      label = c(paste0("Slope = ", round(slope, digits = 2)),
                paste0("Intercept = ", round(intercept, digits = 1)),
                paste0("R\U00B2 = ", round(r_squared, digits = 3))) )

  # ----- Construct Plot -------------------------------------------------------

  if ( showPlot ) {

    timezone <- pat$meta$timezone[1]
    year <- strftime(pat$data$datetime[1], "%Y", tz=timezone)

    # LH Linear regression plot
    lr_plot <-
      both_data %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$pa_pm25, y = .data$pwfsl_pm25)) +
      ggplot2::geom_point(size = size,
                          shape = lr_shape,
                          color = lr_color,
                          alpha = alpha) +
      ggplot2::geom_smooth(method = "lm", size = 0, alpha = 0.45) +
      ggplot2::stat_smooth(geom = "line", color = lr_lcolor, alpha = lr_lalpha,
                           method = "lm", size = lr_lwd) +
      ggplot2::labs(title = "Correlation",
                    x = paste0("PurpleAir: \"", pat$meta$label, "\""),
                    y = paste0("PWFSL: ", monitorID)) +
      ggplot2::theme_bw() +
      ggplot2::xlim(xylim) +
      ggplot2::ylim(xylim) +
      ggplot2::coord_fixed() +    # square aspect ratio
      equationLabel

    plot <- lr_plot

  }

  return(plot)

}

# Show progress function
showLoad <-
  function(FUN) {

    shiny::withProgress(
      expr = FUN,
      value = 0.3,
      message = "Loading...",
      detail = "Please Wait."
    )

    return(FUN)

  }

# Error message handling
handleError <-
  function(expr, msg) {

    shiny::validate(
      shiny::need(expr, msg)
    )

  }

