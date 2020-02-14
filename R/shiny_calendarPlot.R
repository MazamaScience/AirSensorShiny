shiny_calendarPlot <-
  function(pat, tz = NULL) {

    # ===== MAKE THE PLOTLY ====================================================

    # --- Create an interactive plotly for SHINY! ---

    sensor <- pat_createAirSensor(pat)
    gg_cal <- AirMonitorPlots::monitor_ggCalendar(sensor) + scale_fill_sqamd()
    PM2.5 <- signif(x = gg_cal$data$pm25, digits = 2)
    gg_cal <- gg_cal + ggplot2::aes(pm = PM2.5)
    pp_cal <- plotly::ggplotly(p = gg_cal, tooltip = c("pm")) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout( xaxis  = list(fixedrange = TRUE),
                      xaxis2 = list(fixedrange = TRUE),
                      xaxis3 = list(fixedrange = TRUE),
                      xaxis4 = list(fixedrange = TRUE),
                      yaxis  = list(fixedrange = TRUE),
                      yaxis2 = list(fixedrange = TRUE),
                      yaxis3 = list(fixedrange = TRUE),
                      margin = list("r" = 150) )

    return(pp_cal)

  }
