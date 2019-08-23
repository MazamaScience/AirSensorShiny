# Daily patterns

shiny_diurnalPattern <-
  function(pat) {

    logger.debug('----- shiny_diurnalPattern() -----')

    sensor <- AirSensor::pat_createAirSensor(pat)

   p1 <- AirMonitorPlots::ggplot_pm25Diurnal(ws_data = sensor, offsetBreaks = TRUE) +
      AirMonitorPlots::stat_meanByHour() +
      ggplot2::facet_grid(rows = ggplot2::vars(monitorID)) +
      scale_fill_sqamd() # NOT WORK

   p2 <- AirMonitorPlots::monitor_ggClockPlot(sensor) +
     scale_fill_sqamd(reverse = TRUE)


   AirSensor::multi_ggplot(plotList = list(p1, p2),cols = 2)
  }

