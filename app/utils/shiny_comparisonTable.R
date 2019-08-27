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
    recovered <- `/` (nrow(df), nrow(complete_df))

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
