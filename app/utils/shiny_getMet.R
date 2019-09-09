# Get world met data
shiny_getMet <-
  function(label, startdate, enddate ) {

    result <- try({

      logger.trace("loading wind data")

      # Find wind data readings from the closest NOAA site
      year <- lubridate::year(enddate)
      lon <- PAS$longitude[grepl(label, PAS$label)]
      lat <- PAS$latitude[grepl(label, PAS$label)]
      tz <- PAS$timezone[grepl(label, PAS$label)]

      closestSite <- worldmet::getMeta(lon = lon, lat = lat, n = 1,
                                       plot = FALSE)[1,]
      siteCode <- paste0(closestSite$USAF, "-", closestSite$WBAN)

      siteData <-
        worldmet::importNOAA(code = siteCode, year = year, parallel = TRUE) %>%
        dplyr::filter(date >= startdate, date <= enddate)
    }, silent = TRUE)

    timeRange <- range(siteData$date)
    logger.trace("windData goes from %s to %s local time",
                 strftime(timeRange[1]), #tz = tz),
                 strftime(timeRange[2])#, tz = tz)
    )

    # TODO:  Opporutnity to test for time range overlap with sensor data

    # Return the site data from world met
    return(siteData)

  }
