shiny_dySummary <-
  function(
    pat = NULL,
    parameter = "pm25",
    sampleSize = 5000,
    title = NULL,
    xlab = NULL,
    ylab = NULL,
    tlim = NULL,
    rollPeriod = 1,
    showLegend = TRUE,
    colors = NULL
  ) {

    # Remove any duplicate data records
    pat <- pat_distinct(pat)

    # ----- Reduce large datasets by sampling ------------------------------------

    if ( !is.null(sampleSize) ) {

      if ( sampleSize > 1 ) {
        pat <-
          pat %>%
          pat_sample(sampleSize = sampleSize)
      } else {
        pat <-
          pat %>%
          pat_sample(sampleFraction = sampleSize)
      }

    }

    # Convert tlim to POSIXct
    if ( !is.null(tlim) ) {
      dateWindow <- PWFSLSmoke::parseDatetime(tlim)
    } else {
      dateWindow <- NULL
    }

    # Set timezone
    tzCount <- length(unique(pat$meta$timezone))
    if (tzCount > 1) {
      warning(paste0(tzCount, " timezones found. Using UTC time."))
      tzone <- "UTC"
    } else {
      tzone <- unique(pat$meta$timezone)
    }

    sensor <- pat_createAirSensor(pat)

    # Access time

    datetime <- sensor$data[[1]]
    label <- names(sensor$data)[2]

    # Create dygraph
    makeGraph <-
      function(timeseriesMatrix) {

        if ( is.null(title) )( title <- label )

        graph <-
          dygraphs::dygraph(
            timeseriesMatrix,
            main = title,
            xlab = xlab,
            ylab = ylab
          ) %>%
          dygraphs::dyOptions(
            includeZero = T,
            strokeWidth = 2,
            useDataTimezone = TRUE,
            axisLineColor = "white",drawPoints = F
          ) %>%
          dygraphs::dyAxis(rangePad = 15, name = "x") %>%
          dygraphs::dyLegend(hideOnMouseOut = TRUE, show = "onmouseover")

        scaqmd_cols =rev(
          c(
            "PM2.5: >75" = "#6A367A",
            "PM2.5: 55-75" = "#8659A5",
            "PM2.5: 35-55" = "#286096",
            "PM2.5: 12-35" ="#118CBA",#"#3b8aff",
            "PM2.5: 0-12" = "#abe3f4"
          ))
        for ( i in colnames(timeseriesMatrix))
          graph <-
            graph %>%
            dygraphs::dyStackedBarGroup(name=i, color=scaqmd_cols[i])

        return(graph)

      }

    ct <- cut(sensor$data[[2]], c(0,12,35,55,77, 99999))
    groups <- list()

    for ( lvl in levels(ct) ) {

      g <- na.omit(sensor$data[ ct == lvl,])
      groups[[lvl]] <- xts::xts(x = g[[2]], order.by = g[[1]], tzone = tzone)

    }


    timeseriesMatrix <-
      cbind( groups[[1]],
             groups[[2]],
             groups[[3]],
             groups[[4]],
             groups[[5]]
      )

    colnames(timeseriesMatrix) <-
      c( "PM2.5: 0-12",
         "PM2.5: 12-35",
         "PM2.5: 35-55",
         "PM2.5: 55-75",
         "PM2.5: >75" )[1:length(colnames(timeseriesMatrix))]

    if ( is.null(ylab) )( ylab <- "\u03bcg / m\u00b3" )



    return( makeGraph(timeseriesMatrix) )

  }
