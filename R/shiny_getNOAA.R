# Get world met data
shiny_getNOAA <-
  function(sensor, startdate = NULL, enddate = NULL, tz = NULL) {
    logger.trace("loading wind data")

    # Find wind data readings from the closest NOAA site
    year <- lubridate::year(enddate)
    lon <- sensor$meta$longitude
    lat <- sensor$meta$latitude

    startdate <- lubridate::ymd(startdate, tz = tz)
    enddate <- lubridate::ymd(enddate, tz = tz)

    # if ( is.null(startdate) ) startdate <- lubridate::ymd(range(sensor$data$datetime)[1])
    # if ( is.null(enddate) ) enddate <- lubridate::ymd(range(sensor$data$datetime)[2])

    closestSite <- worldmet::getMeta(lon = lon, lat = lat, n = 1,
                                     plot = FALSE)[1,]
    siteCode <- paste0(closestSite$USAF, "-", closestSite$WBAN)

    siteData <- worldmet::importNOAA(code = siteCode, year = year, parallel = TRUE) %>%
      dplyr::filter(date >= startdate, date <= enddate)

    timeRange <- range(siteData$date)
    logger.trace("windData goes from %s to %s local time",
                 strftime(timeRange[1], tz = tz),
                 strftime(timeRange[2], tz = tz))
    return(siteData)
  }
