# Get world met data
shiny_getNOAA <-
  function(sensor, startdate, enddate ) {
    logger.trace("loading wind data")

    # Find wind data readings from the closest NOAA site
    year <- lubridate::year(enddate)
    lon <- sensor$meta$longitude
    lat <- sensor$meta$latitude

    closestSite <- worldmet::getMeta(lon = lon, lat = lat, n = 1,
                                     plot = FALSE)[1,]
    siteCode <- paste0(closestSite$USAF, "-", closestSite$WBAN)

    siteData <- worldmet::importNOAA(code = siteCode, year = year, parallel = TRUE) %>%
      dplyr::filter(date >= startdate, date <= enddate)

    timeRange <- range(siteData$date)
    logger.trace("windData goes from %s to %s local time",
                 strftime(timeRange[1]),
                 strftime(timeRange[2]))
  }
