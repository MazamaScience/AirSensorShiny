shiny_comparisonTable <-
  function(pat) {

    logger.debug('----- shiny_comparisonTable() -----')

    # ----- Validate parameters --------------------------------------------------

    if ( !AirSensor::pat_isPat(pat) )
      stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")

    if ( AirSensor::pat_isEmpty(pat) )
      stop("Parameter 'pat' has no data.")

    # simplify dataframe
    df <- pat$data

    # Fill the missing date rows with NA
    complete_df <-
      tidyr::complete(
        data = df,
        datetime = seq(
          from = head(df$datetime, n=1),
          to = tail(df$datetime, n=1),
          by = "2 min"
        )
      )

    # Calc recovered ratio
    recovered <- (nrow(df) / nrow(complete_df))

    # define table
    comparisonTable <-
      data.frame(
        pat$meta$label,
        nrow(df),
        recovered*100
      )

    names(comparisonTable) <- c("Sensor", "Measurements", "Percent Recovered")

    return(comparisonTable)
  }

shiny_metTable <-
  function(metData) {

  if (TRUE)
    metData <-
      shiny_getMet("SCSC_33", lubridate::ymd("20190820"), lubridate::ymd("20190827"))

  metTable <-
    data.frame(
      mean(metData$wd, na.rm = TRUE),
      mean(metData$ws, na.rm = TRUE),
      mean(metData$RH, na.rm = TRUE),
      mean(metData$air_temp, na.rm = TRUE)
  )
  names(metTable) <- c("Wind Direction (deg)", "Wind Speed (m/s)", "Humidity (%)", "Temperature (C)")

  return(metTable)

  }
