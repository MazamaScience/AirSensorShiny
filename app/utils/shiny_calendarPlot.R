shiny_calendarPlot <-
  function(pat) {

    # ====== TEMPORARY INTERNAL CALENDAR PLOT FUNCTION =========================
    .sensor_calendarPlot <- function(
      sensor = NULL,
      palette = NULL,
      ncol = 3,
      aspectRatio = 4/5,
      discrete = TRUE
    ) {

      # ----- Validate parameters ----------------------------------------------

      if ( sensor_isEmpty(sensor) )
        stop("Parameter 'sensor' is not a valid 'airsensor' object.")

      if ( sensor_isEmpty(sensor) )
        stop("Parameter 'sensor' has no data.")

      if ( nrow(sensor$meta) > 1 )
        stop("Parameter 'sensor' must contain data for only one sensor.")

      # Always specify local timezones!
      timezone <- sensor$meta$timezone

      # Create data frame
      df <- sensor$data

      # Fill missing dates # CHECK IF LUBRIDATE CAN BE USED
      df <-
        tidyr::complete(
          data = df,
          datetime = seq(
            from = as.POSIXct(
              paste0(
                strftime(
                  df$datetime,
                  format = "%Y",

                  tz = timezone)[2],
                "-01-01"
              ) ,
              tz = strftime(
                df$datetime,
                format = "%Z",
                tz = timezone)[1]
            ),
            to = as.POSIXct(
              paste0(
                strftime(
                  df$datetime,
                  format = "%Y",
                  tz = timezone)[2],
                "-12-31")
            ),
            by = "1 day"
          )
        )

      # ----- Prepare plot data ------------------------------------------------
      # Rename the data column to "pm25"
      names(df)[2] <- "pm25"

      # Create calendar plot handler data frame
      df$datetime <- zoo::as.Date(df$datetime)  # format date
      df$day <- as.numeric(strftime(df$datetime, format = "%d"))
      df$yearmonth <- zoo::as.yearmon(df$datetime)
      df$yearmonthf <- factor(df$yearmonth)
      df$week <- as.numeric(strftime(df$datetime, format = "%W"))
      df$year <- as.numeric(strftime(df$datetime, format = "%Y"))
      df$month <- as.numeric(strftime(df$datetime, format = "%m"))
      df$monthf <- months.Date(df$datetime, abbreviate = TRUE)
      df$weekdayf <- weekdays.Date(df$datetime, abbreviate = TRUE)
      df$weekday <- as.numeric(strftime(df$datetime, format = "%d"))
      df$weekd <- ordered(df$weekdayf,
                          levels= c( "Mon",
                                     "Tue",
                                     "Wed",
                                     "Thu",
                                     "Fri",
                                     "Sat",
                                     "Sun" )
      )

      df$monthweek <- as.numeric(NA) # placeholder

      # Compute week number for each month
      df <-
        df %>%
        plyr::ddply(
          .variables = plyr::.(yearmonthf),
          .fun = transform,
          monthweek = 1 + week - min(week)
        )

      # Capture only whats needed
      df <-
        df[, c( "year",
                "yearmonthf",
                "monthf",
                "week",
                "monthweek",
                "weekdayf",
                "weekd",
                "day",
                "pm25"
        )]

      # Create the title
      title <- paste0(
        unique(range(df$year)),
        ": ",
        sensor$meta$monitorID
      )

      # Determine fill type
      if ( discrete ){
        fill <-
          cut(
            df$pm25,
            breaks = c(0,12, 35, 55, 75, 6000),
            labels = c("0-12", "12-35", "35-55", "55-75", ">75")
          )
      } else {
        fill = df$pm25
      }

      # ----- Create plot ------------------------------------------------------

      gg <-
        ggplot2::ggplot(
          df,
          ggplot2::aes(
            stats::reorder(.data$monthweek, dplyr::desc(.data$monthweek)),
            .data$weekd,
            fill = fill
          )
        ) +
        ggplot2::geom_tile(color = "grey88", size=0.5) +
        ggplot2::facet_wrap(drop = TRUE, ncol = ncol, dir = "h",
                            factor(monthf, levels = month.abb) ~ .
        ) +
        ggplot2::labs(
          title = title,
          fill="PM2.5 (\u03bcg / m\u00b3)") +
        ggplot2::geom_text(
          ggplot2::aes(label=.data$day),
          size = 3,
          fontface = "bold"
        ) +
        ggplot2::theme_classic() +
        ggplot2::coord_flip() +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 7),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          legend.position = "bottom",
          aspect.ratio = aspectRatio,
          legend.text = ggplot2::element_text(size="8")
        ) +
        ggplot2::scale_fill_discrete(na.value="white")

      # ----- Return -----------------------------------------------------------

      return(gg)

    }

    # ====== TEMPORARY INTERNAL CALENDAR PLOT FUNCTION =========================
    .pat_calendarPlot <- function(
      pat = NULL,
      palette = NULL,
      ncol = 3,
      aspectRatio = 4/5,
      channel = "ab",
      qc_algorithm = "hourly_AB_01",
      min_count = 20,
      discrete = TRUE
    ) {

      # ----- Validate parameters ----------------------------------------------

      channel <- tolower(channel)

      MazamaCoreUtils::stopIfNull(pat)

      if ( !AirSensor::pat_isPat(pat) )
        stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")

      if ( AirSensor::pat_isEmpty(pat) )
        stop("Parameter 'pat' has no data.")

      # Remove any duplicate data records
      pat <- AirSensor::pat_distinct(pat)

      # ----- Create hourly aggregated data ------------------------------------

      sensor <-
        AirSensor::pat_createAirSensor(
          pat,
          period = "day",
          parameter = "pm25",
          channel = channel,
          qc_algorithm = qc_algorithm,
          min_count = min_count
        )

      # ----- Create plot ------------------------------------------------------

      gg <- .sensor_calendarPlot(sensor, palette, ncol, discrete = discrete)

      # ----- Return -----------------------------------------------------------

      return(gg)
    }


    # ===== MAKE THE PLOTLY ====================================================

    # --- Create an interactive plotly for SHINY! ---

    gg_cal <- .pat_calendarPlot(pat = pat, ncol = 4) + scale_fill_sqamd()
    PM2.5 <- signif(x = gg_cal$data$pm25, digits = 3)
    gg_cal <- gg_cal + ggplot2::aes(pm = PM2.5)

    pp_cal <- plotly::ggplotly(p = gg_cal, tooltip = "pm") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout( xaxis  = list(fixedrange = TRUE),
                      xaxis2 = list(fixedrange = TRUE),
                      xaxis3 = list(fixedrange = TRUE),
                      xaxis4 = list(fixedrange = TRUE),
                      yaxis  = list(fixedrange = TRUE),
                      yaxis2 = list(fixedrange = TRUE),
                      yaxis3 = list(fixedrange = TRUE),
                      margin = list("r" = 150) )

    return(pp_cal)

  }
