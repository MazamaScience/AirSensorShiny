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


shiny_externalFit <-
  function(
  sensor = NULL,
  startdate = NULL,
  enddate = NULL,
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
  min_count = 20,
  tz = NULL
) {

  logger.debug('----- shiny_externalFit() -----')

  # ----- Validate parameters --------------------------------------------------

  if ( !PWFSLSmoke::monitor_isMonitor(sensor) ) {
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  }
  if ( PWFSLSmoke::monitor_isEmpty(sensor) ) {
    stop("Parameter 'pat' has no data.")
  }

  # Crop to dates

  sensor <- PWFSLSmoke::monitor_subset(ws_monitor = sensor, tlim = c(startdate, enddate))

  # For easier access
  # meta <- sensor$meta
  # data <- sensor$data

  # ----- Assemble data ---------------------------------------------

  paHourly_data <- PWFSLSmoke::monitor_extractData(sensor)

  names(paHourly_data) <- c("datetime", "pa_pm25")

  # Get the PWFSL monitor data
  monitorID <- sensor$meta$pwfsl_closestMonitorID
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

    timezone <- tz
    year <- strftime(sensor$data$datetime[1], "%Y", tz=timezone)

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
                    x = paste0("PurpleAir: \"", sensor$meta$monitorID, "\""),
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
