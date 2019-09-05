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
      AirMonitorPlots::stat_meanByHour(output = "scaqmd")

   return(p1)

  }

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  sensor <- example_sensor

}

