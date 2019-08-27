# Daily patterns

shiny_diurnalPattern <-
  function(pat) {

    logger.debug('----- shiny_diurnalPattern() -----')

    sensor <- AirSensor::pat_createAirSensor(pat)

    p1 <-
      AirMonitorPlots::ggplot_pm25Diurnal(
        ws_data = sensor,
        offsetBreaks = TRUE
      ) +
      stat_meanByHour() +
      scale_fill_sqamd()

    p2 <- AirMonitorPlots::monitor_ggClockPlot(sensor) + scale_fill_sqamd()


    AirSensor::multi_ggplot(plotList = list(p1, p2),cols = 2)
  }

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  sensor <- example_sensor

}

