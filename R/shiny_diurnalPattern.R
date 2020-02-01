# Daily patterns

shiny_diurnalPattern <-
  function(
    sensor = NULL,
    startdate = NULL,
    enddate = NULL
           ) {

    logger.debug('----- shiny_diurnalPattern() -----')

    # sensor <- AirSensor::pat_createAirSensor(pat)

    sensor <-
      PWFSLSmoke::monitor_subset(
        sensor,
        tlim = c(startdate, enddate),
        timezone = sensor$meta$timezone
      )

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

