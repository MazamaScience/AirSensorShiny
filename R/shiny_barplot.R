#' @export
#' @title Bar plot for AirSensorShiny
#'
#' @param pat PurpleAir Timeseries "pat" object from \code{pat_createNew()}
#' @param period The time period to average to. Can be "sec", "min", "hour",
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param startdate The start date. Used to provide the Datetime domain.
#' @param enddate The end date. Used to provide the Datetime domain.
#' @param ylim Y-axis limits.
#'
#' @description A barplot that bins by period and preforms an average of channel
#' A & B, or seperately. Intended for use with the AirSensorShiny web app.
#' @details The \code{ylim} is used to fix the Y-axis domain in order to have
#' consistent visualization.The X-axis is determined by the
#' datetime interval.
#'
#'

shiny_barplot <-
  function(
    pat,
    period,
    startdate,
    enddate,
    ylim = NULL
  ) {

    logger.debug('----- shiny_barplot() -----')

    # ----- Validate parameters ------------------------------------------------

    if ( AirSensor::pat_isEmpty(pat) )
      stop("Required Purple Air time series is missing")

    if ( !AirSensor::pat_isPat(pat) )
      stop("Required parameter 'pat' is not a valid 'pa_timerseries' object.")

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
      #ggplot2::scale_fill_viridis_d(begin = 0.15) +
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

    cuts <- cut(ast$data[[2]], breaks = c(0,12, 35, 55, 75, 6000))

    # Average PM2.5 barplot
    pm25_avg_bar <-
      ggplot2::geom_bar(
        data = ast$data,
        mapping = ggplot2::aes(
          x = .data$datetime,
          y = ast$data[[2]],
          fill = sqamd_break(ast$data[[2]])),

        stat = "identity",
        color = "gray95",
        show.legend = FALSE
      )

    gg <-
      pm25_plot +
      pm25_avg_bar +
      scale_fill_sqamd()


    return(gg)

  }
