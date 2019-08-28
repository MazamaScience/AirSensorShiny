# Daily patterns

shiny_diurnalPattern <-
  function(pat) {

    if (TRUE) {
      sensor <- example_sensor
    }

    logger.debug('----- shiny_diurnalPattern() -----')

    sensor <- AirSensor::pat_createAirSensor(pat)

   p1 <- AirMonitorPlots::ggplot_pm25Diurnal(ws_data = sensor, offsetBreaks = TRUE) +
     AirMonitorPlots::stat_meanByHour(output = "scaqmd")

   p2 <- AirMonitorPlots::monitor_ggClockPlot(sensor) + scale_fill_sqamd()


   AirSensor::multi_ggplot(plotList = list(p1, p2),cols = 2)
  }

