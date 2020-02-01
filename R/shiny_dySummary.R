shiny_dySummary <-
  function(
    sensor = NULL,
    startdate = NULL,
    enddate = NULL,
    parameter = "pm25",
    sampleSize = 5000,
    title = NULL,
    tlim = NULL,
    rollPeriod = 1,
    showLegend = TRUE,
    colors = NULL
  ) {

    # Convert tlim to POSIXct
    if ( !is.null(tlim) ) {
      dateWindow <- PWFSLSmoke::parseDatetime(tlim)
    } else {
      dateWindow <- NULL
    }

    # Set timezone
    tzCount <- length(unique(sensor$meta$timezone))
    if (tzCount > 1) {
      warning(paste0(tzCount, " timezones found. Using UTC time."))
      tzone <- "UTC"
    } else {
      tzone <- unique(sensor$meta$timezone)
    }

    sensor <-
      PWFSLSmoke::monitor_subset(
      sensor,
      tlim = c(startdate, enddate)
    )

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
            xlab = "Date",
            ylab = "PM<sub>2.5</sub> (\u03bcg / m\u00b3)"
          ) %>%
          dygraphs::dyOptions(
            includeZero = FALSE,
            strokeWidth = 2,
            useDataTimezone = TRUE,
            axisLineColor = "white",
            drawPoints = FALSE
          ) %>%
          dygraphs::dyUnzoom() %>%
          dygraphs::dyAxis(rangePad = 15, name = "x") %>%
          dygraphs::dyLegend(hideOnMouseOut = TRUE, show = "onmouseover")

        scaqmd_cols =rev(
          c(
            ">75 \u03bcg / m\u00b3" = "#6A367A",
            "55-75 \u03bcg / m\u00b3" = "#8659A5",
            "35-55 \u03bcg / m\u00b3" = "#286096",
            "12-35 \u03bcg / m\u00b3" ="#118CBA",
            "0-12 \u03bcg / m\u00b3" = "#abe3f4"
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
      c( "0-12 \u03bcg / m\u00b3",
         "12-35 \u03bcg / m\u00b3",
         "35-55 \u03bcg / m\u00b3",
         "55-75 \u03bcg / m\u00b3",
         ">75 \u03bcg / m\u00b3" )[1:length(colnames(timeseriesMatrix))]

    return( makeGraph(timeseriesMatrix) )

  }
