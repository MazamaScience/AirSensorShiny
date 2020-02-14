shiny_barplotly <-
  function( sensor, startdate = NULL, enddate = NULL, ylim = NULL, tz = NULL) {
    if (FALSE) {
      sensor <- AirSensor::sensor_loadLatest(collection = "scaqmd", days = 45) %>%
        PWFSLSmoke::monitor_subset(monitorIDs = "SCSC_33")
      startdate = "2019-09-01"
      enddate = "2019-09-04"
      ylim = NULL
    }

    # Create POSIXct times for ggplot
    xlim <- c(as.POSIXct(lubridate::ymd(startdate, tz = tz), tz = tz), as.POSIXct( lubridate::ymd(enddate, tz = tz), tz = tz))
    if ( grepl("-", startdate ) | grepl("-", enddate) ) {
      startdate <- stringr::str_remove_all(startdate, "-")
      enddate <- stringr::str_remove_all(enddate, "-")
    }

    sensor$data$datetime <- lubridate::with_tz(sensor$data$datetime, tz)
    sensor <- PWFSLSmoke::monitor_subset(sensor, tlim = c(startdate, enddate))

    cts <- cut(sensor$data[[2]], breaks = c(0,12,35,55,75,1000))

    label <- names(sensor$data)[2]

    ddif <- lubridate::ymd(enddate, tz = tz) - lubridate::ymd(startdate, tz = tz)

    # Tooltip labels
    PM2.5 <- signif(sensor$data[[label]], digits = 3)
    Date <- lubridate::ymd_hms(sensor$data[["datetime"]], tz = tz)


    gg <-
      ggplot2::ggplot(
        data = sensor$data,
        mapping = ggplot2::aes( x  = .data[["datetime"]],
                                pm =  PM2.5,
                                date = Date ),
        y =.data[[label]],
        color = .data[[label]]
      ) +
      ggplot2::geom_bar(
        mapping = ggplot2::aes( x = .data[["datetime"]],
                                y = .data[[label]],
                                fill = cts ),
        stat = "identity",
        size = ifelse(ddif < 15, 0.8, 0),
        color = "white"
      ) +
      scale_fill_sqamd() +
      ggplot2::scale_x_datetime(breaks = "1 day", date_labels = '%b %d') +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text( size = 14,
                                            face = "bold",
                                            hjust = 0.5,
                                            margin = ggplot2::margin(b=0) ),
        axis.text.x = ggplot2::element_text(
          angle = ifelse(ddif > 10, 25, 0),
          hjust = 1,
          size = 11),
        axis.text.y = ggplot2::element_text(size = 11),
        axis.title.x = ggplot2::element_text(size = 12),
        axis.title.y = ggplot2::element_text(size = 12),
        legend.position = "none",
        panel.grid.major.y = ggplot2::element_line(color = "grey52"),
        panel.grid.major.x = ggplot2::element_line(color = "grey52")
      ) +
      ggplot2::ggtitle(label = label) +
      ggplot2::coord_cartesian(ylim = ylim) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("(\u03bcg / m\u00b3)")

    pp <-
      plotly::ggplotly(gg, tooltip = c("date", "pm")) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(yaxis = list( fixedrange = TRUE,
                                   autorange = TRUE,
                                   title = "PM<sub>2.5</sub> (\u03bcg / m\u00b3)",
                                   titlefont = list(size = 14.5)),
                     xaxis = list(fixedrange = F, autorange = T))#TRUE))

    return(pp)
  }

