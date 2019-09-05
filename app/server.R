# ----- AirShiny Server Logic --------------------------------------------------

#shiny::shinyServer(
server <-
  function(input, output, session) {

    # ----- Reactive Controls --------------------------------------------------

    # Define active user selections
    # NOTE: This contains all active data to avoid redundant func and load.
    #       Update the active values only on trigger events.
    active <-
      shiny::reactiveValues(
        pas = NULL,
        pat = NULL,
        label = NULL,
        enddate = NULL,
        community = NULL,
        lookback = NULL,
        marker = NULL,
        compmarker = NULL,
        tab = NULL,
        navtab = NULL,
        exp_label = NULL,
        latest_load = NULL,
        latest_community = NULL,
        latest_label = NULL,
        communityId = NULL,
        help = NULL
      )

    # Update the active variable with an input variable
    updateActive <-
      function(update, with) active[[update]] <- input[[with]][1]

    # Update active pas and label
    shiny::observeEvent(
      label = "pas update",
      input$pas_select,
      {
        updateActive("label", "pas_select")
        active$pas <- PAS[grepl(active$label, PAS$label),][1,]
      }
    )

    # Update active community
    shiny::observeEvent(
      label = "community update",
      input$comm_select,
      { updateActive("community", "comm_select")
        active$communityId <-
          names(which(CommunityById == active$community))
      }
    )

    # Update active enddate
    shiny::observeEvent(
      label = "date update",
      input$date_select,
      updateActive("enddate", "date_select")
    )

    # Update active lookback days
    shiny::observeEvent(
      label = "lookback update",
      input$lookback_select,
      updateActive("lookback", "lookback_select")
    )

    # Update active marker with main leaflet
    shiny::observeEvent(
      label = "leaflet marker update",
      input$leaflet_marker_click,
      updateActive("marker", "leaflet_marker_click")
    )

    # Update active tab
    shiny::observeEvent(
      label = "tab select",
      input$tab_select,
      updateActive("tab", "tab_select")
    )

    # Update active marker with comparison leaflet
    shiny::observeEvent(
      label = "comparison leaflet marker update",
      input$shiny_leaflet_comparison_marker_click,
      updateActive("marker", "shiny_leaflet_comparison_marker_click")
    )

    # Update active navtab
    shiny::observeEvent(
      label = "nav tab update",
      input$navtab,
      updateActive("navtab", "navtab")
    )

    # Update data explorer pas selection
    shiny::observeEvent(
      label = "data explorer pas select update",
      input$de_pas_select,
      updateActive("exp_label", "de_pas_select")
    )

    # Update active marker with latest leaflet marker click
    shiny::observeEvent(
      label = "latest map marker update",
      input$latest_leaflet_marker_click,
      updateActive("marker", "latest_leaflet_marker_click")
    )

    # update active latest data community selection
    shiny::observeEvent(
      label = "latest community update",
      input$latest_comm_select,
      updateActive("latest_community", "latest_comm_select")
    )

    # update active latest data pas selection
    shiny::observeEvent(
      label = "latest pas select update",
      input$latest_pas_select,
      updateActive("latest_label", "latest_pas_select")
    )

    # Update active community ID with pas selection
    shiny::observeEvent(
      label = "update community ID",
      input$pas_select,
      { active$communityId <-
        names(
          which(CommunityById == active$pas$communityRegion)
        )
      }
    )

    # Update help text with help click
    shiny::observeEvent(
      input$help_select,
      updateActive("help", "help_select")
    )

    # Update active community with DE pas select
    # shiny::observeEvent(
    #   input$de_comm_select,
    #   updateActive("community", "de_comm_select")
    # )

    # Update date based on DE date select
    shiny::observeEvent(
      input$de_date_select,
      updateActive("enddate", "de_date_select")
    )

    # Update lookback based on DE lookback
    shiny::observeEvent(
      input$de_lookback_select,
      updateActive("lookback", "de_lookback_select")
    )

    # ----- Reactive Functions -------------------------------------------------

    # Get the dates
    getDates <-
      shiny::reactive({

        sd <- lubridate::ymd(active$enddate, tz = TIMEZONE) -
          lubridate::ddays(as.numeric(active$lookback))

        ed <- lubridate::ymd(active$enddate, tz = TIMEZONE)

        return(c(sd, ed))

      }, label = "get dates")

    # Get the valid pas selectables
    getPasLabels <-
      shiny::reactive({

        if ( active$navtab == "latest" ) {
          community <- active$latest_community
        } else {
          community <- active$community
        }

        # NOTE: Remove any pas with no def community & contains B & inside
        if ( community == "all" ) {
          return(PAS$label[!is.na(PAS$communityRegion) &
                             !grepl("(<?\\sB)$", PAS$label) &
                             PAS$DEVICE_LOCATIONTYPE != "inside"])
        } else {
          return(PAS$label[grepl(community, PAS$communityRegion) &
                             !grepl("(<?\\sB)$", PAS$label) &
                             PAS$DEVICE_LOCATIONTYPE != "inside"])
        }

      }, label = "get pas labels")

    # Get the sensors in the selected community
    getCommunitySensors <-
      shiny::reactive({

        community <-
          ifelse(
            active$navtab == "latest",
            active$latest_community,
            active$community
          )

        # NOTE: ifelse function does not work here...
        if ( community == "all" )  {
          pas <- PAS[!is.na(PAS$communityRegion) &
                       !grepl("(<?\\sB)$", PAS$label) &
                       PAS$DEVICE_LOCATIONTYPE != "inside",]
        } else {
          pas <- PAS[grepl(community, PAS$communityRegion),]
        }

        pas$label <-
          stringr::str_split(
            string = pas$label,
            pattern = " ",
            simplify = TRUE
          )[,1]

        return(pas)

      }, label = "get community sensors")

    # Load latest data of the past day from thingspeak on load button press
    thingspeakLatest <-
      shiny::eventReactive(
        input$loadButton,
        {
          ed <- lubridate::now(tzone = TIMEZONE)

          sd <- ed - lubridate::ddays(1)

          # Get both channels from global pas
          pas <- PAS[grepl(active$label, PAS$label),]

          latest <-
            AirSensor::pat_createNew(
              pas=pas,
              label=active$label,
              startdate=sd,
              enddate=ed
            )
        }, label = "load latest data from thingspeak")

    # ----- Render Handling ----------------------------------------------------

    # Render Map
    renderLeaf <-
      function() {

        leaflet::renderLeaflet({

          sensors <- getCommunitySensors()

          # Use the defined pas_valid_choices to apply filters if needed
          # i.e: remove any PAS that contains "Indoor" in its label

          valid_sensors <-
            sensors[which(!stringr::str_detect(sensors$label, "[Indoor]")),]

          shiny_leaflet(
            pas = valid_sensors,
            parameter = "pm25_current",maptype = "Stamen.TonerLite",#"CartoDB.Positron",
            paletteName = "PuBu"#"Spectral" #"Purple"
          )

        })

      }

    # Render Bar Plot
    renderBarPlot <-
      function(plotType) {

        shiny::renderPlot({

          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please Select a Sensor."
          )

          pat <- active$pat
          dates <- getDates()
          if ( plotType == "daily_plot" ) {

            result <-
              try({plot <-
                shiny_barplot(
                  pat,
                  period = "1 day",
                  startdate = dates[1],
                  enddate = dates[2]
                )}, silent = TRUE )

          }  else if ( plotType == "hourly_plot" ) {

            result <-
              try({
                plot <-
                  shiny_barplot(
                    pat,
                    period = "1 hour",
                    startdate = dates[1],
                    enddate = dates[2]
                  )}, silent = TRUE)

          }
          if ( "try-error" %in% class(result) ) {
            notify("Summary Plot Failed.", duration = 15)
          } else {
            return(plot)
          }
        })

      }

    # Render calendar plot
    renderCalPlot <-
      function() {

        shiny::renderPlot({

          # Require active label before loading
          shiny::req(active$label)

          # Create dates from start of year to enddate
          sd <- lubridate::floor_date(active$enddate, "year")
          # NOTE: Use last seen date to insure calendar plot has all annual data
          ed <- lubridate::date(active$pas$lastSeenDate)

          showLoad({

            # Load annual pat to now
            pat <-
              try(
                AirSensor::pat_load(
                  active$label,
                  startdate = sd,
                  enddate = ed
                )
              )

            if ( "try-error" %in% class(pat) ) {

              handleError("", "Error: Please select a different sensor.")
              notify("Data Creation Failed")

            }

            shiny::incProgress(0.65)

            # Calendar plot
            calendar <-
              try({
                AirSensor::pat_calendarPlot(pat, ncol = 2) +
                  scale_fill_sqamd()
              })

            if ( "try-error" %in% class(calendar) ) {

              notify("Calendar Creation Failed")
              handleError("", paste0(active$label, ": Calendar Unavailable"))

            }

          })

          return(calendar)
        })

      }

    # Render Selected mini table
    renderMiniTable <-
      function() {

        shiny::renderTable({

          #req(active$label, active$pas)
          if ( is.null(active$label) ||
               active$label == "Select Sensor..." ||
               active$label == "") {

            mini <-
              dplyr::tibble(
                #"Sensor" = "",
                "Latitude" ="",
                "Longitude" = ""
              )

          } else {

            mini <-
              dplyr::tibble(
                #"Sensor" = active$label,
                "Latitude" = active$pas$latitude,
                "Longitude" = active$pas$longitude
              )

          }
          return(mini)

        },align = "c", spacing = "xs")

      }

    # Render the monitor comparison plot
    renderMonitorComp <-
      function() {

        shiny::renderPlot({

          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please select a sensor to compare with the nearest monitor."
          )

          req(active$pat)

          showLoad({

            shiny::incProgress(0.66)

            AirSensor::pat_monitorComparison(active$pat)

          })

        })

      }

    # Render monitor & pas external fit plot
    renderExtFit <-
      function() {

        shiny::renderPlot({

          req(active$pat)

          shiny_externalFit(active$pat)

        })

      }

    # Render Multiplot
    renderMultiplot <-
      function(columns = NULL) {

        shiny::renderPlot({

          if ( is.null(columns) ) columns <- 2

          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please select a sensor."
          )

          shiny::req(active$pat)

          AirSensor::pat_multiplot(active$pat, columns = columns)

        })

      }

    # Render rose plot
    renderRose <-
      function() {

        shiny::renderPlot({

          shiny::req(active$label)

          showLoad({

            dates <- getDates()

            # load sensor
            sensor <- AirSensor::sensor_load(
              startdate = dates[1],
              enddate = dates[2]
            )

            logger.trace("sensor_load(%s, %s) returns %d rows of data",
                         strftime(dates[1]),#, tz = sensor$meta$timezone),
                         strftime(dates[2]), #,tz = sensor$meta$timezone),
                         nrow(sensor$data))

            # Filter sensor
            sensor <-
              sensor %>%
              AirSensor::sensor_filterMeta(monitorID == active$label)

            logger.trace("sensor '%s' has %s rows of data",
                         active$label, nrow(sensor$data))

            # Get world met data
            metData <-
              shiny_getMet(
                label=active$label,
                startdate=dates[1],
                enddate=dates[2]
              )

            # filter wind data
            windData <- dplyr::select(metData, c("date", "wd", "ws"))

            result <- try({
              rose <- AirSensor::sensor_pollutionRose(sensor, windData)
            }, silent = FALSE)

            if ( "try-error" %in% class(result) ) {

              notify("Rose Plot Failed")
              handleError("", paste0(active$label, ": Rose Plot Unavailable"))

            }

            logger.trace("class(rose) = %s", class(rose))

            return(rose)

          })

        })

      }

    # Render video
    renderVideo <-
      function() {

        shiny::renderUI({

          baseUrl <-
            "http://smoke.mazamascience.com/data/PurpleAir/videos/"
          year <-
            strftime(active$enddate, "%Y", tz = TIMEZONE)
          mm <-
            strftime(active$enddate, "%m", tz = TIMEZONE)
          dd <-
            strftime(active$enddate, "%d", tz = TIMEZONE)

          comm <- active$communityId

          url <- paste0(baseUrl, year, "/", comm, "_", year, mm, dd, ".mp4" )

          tags$video(
            id="video2",
            type = "video/mp4",
            src = url,
            controls = "controls",
            loop = TRUE
          )

        })

      }

    # Render Meta table explorer
    renderMetaExplorer <-
      function() {

        shiny::renderTable({

          req(active$pas, active$label)

          pat <- active$pat

          community <- active$pas$communityRegion

          meta <-
            data.frame(
              "Community" = community,
              "Sensor Type" = active$pas$sensorType,
              "Longitude" = active$pas$longitude,
              "Latitude" = active$pas$latitude,
              "State" = active$pas$stateCode,
              "Country" = active$pas$countryCode,
              "Timezone" = active$pas$timezone
            )

          return(meta)

        })

      }

    # Render Data Table Explorer
    renderDataExplorer <-
      function() {

        DT::renderDataTable({

          # handleError(
          #   AirSensor::pat_isPat(active$pat),
          #   "Please select a sensor."
          # )
          #
          req(active$pat)
          #
          showLoad(shiny::incProgress(0.7))

          # Remove unecessary columns
          data <- active$pat$data[-(6:10)]

          names(data) <- c( "Datetime",
                            "PM2.5 Ch. A (\u03bcg / m\u00b)",
                            "PM2.5 Ch. B (\u03bcg / m\u00b)",
                            "Temperature (F)",
                            "Relative Humidity (%)" )

          data <-
            DT::datatable(data, selection = "none") %>%
            DT::formatDate(1, method = 'toLocaleString', params = list('en-EN'))

          return(data)

        })

      }

    renderPatternPlot <-
      function() {

        shiny::renderPlot({

          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please select a sensor."
          )
          req(active$pat)

          showLoad({

            shiny::incProgress(0.7)

            shiny_diurnalPattern(active$pat)


          })

        })

      }

    renderDygraphPlot <-
      function() {

        dygraphs::renderDygraph({

          AirSensor::pat_dygraph(
            pat = active$pat,
            sampleSize = NULL
          )

        })

      }

    renderAuxPlot <-
      function() {

        shiny::renderPlot({

          showLoad({

            latest <- thingspeakLatest()

            shiny::incProgress(0.666)

            AirSensor::pat_multiplot(
              pat = latest,
              plottype = "all", #"aux",
              sampleSize = NULL,
              columns = 1
            )


          })

        })

      }

    renderCompTable <-
      function() {

        shiny::renderTable({
          shiny::req(active$pat)
          shiny_comparisonTable(active$pat)

        }, colnames = TRUE, align = "c", bordered = TRUE)

      }

    renderMetTable <-
      function() {

        shiny::renderTable({
          showLoad({
            shiny::req(active$pat, active$label)
            dates <- getDates()
            metData <- shiny_getMet(active$label, dates[1], dates[2])

            table <- shiny_metTable(metData)

            shiny::incProgress(0.6)
          })

          return(table)

        }, bordered = TRUE, align = "c")

      }

    renderDygraphSummary <-
      function() {

        dygraphs::renderDygraph({

          shiny::req(active$pat)
          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please Select a Sensor."
          )

          shinyjs::reset("dySummary_plot")

          result <-
            try({dySummary <- shiny_dySummary(active$pat)}, silent = TRUE)

          if ( "try-error" %in% class(result) ) {
            notify("Summary Failed")
            handleError("", paste0(active$label, ": Failed"))
          }

          return(dySummary)

        })

      }

    # ----- Helper functions ---------------------------------------------------

    # Handle download button
    downloadButton <-
      function() {

        shiny::downloadHandler(
          filename = function() {

            dates <- getDates()

            paste0(
              active$pat$meta$label,
              "_",
              dates[1],
              "_",
              dates[2],
              ".csv"
            )

          },

          content = function(file) {

            pat <- active$pat
            write.csv(pat$data, file = file)

          }

        )

      }

    # Update leaflet function for selected pas, or map click
    updateLeaf <-
      function(label = NULL) {

        # If on main tab -> update main leaflet
        if ( active$tab == "main" || active$navtab == "latest") {

          # Get data
          if ( is.null(label) ) ( label <- active$marker )
          ind <- which(PAS$label == label)
          lng <- PAS$longitude[ind]
          lat <-  PAS$latitude[ind]

          # Create popup HTML
          html <-
            list(
              "label" = paste("<b><a>",label,"</a></b>"),
              "seen" = PAS$statsLastModifiedDate[ind],
              "pm" = paste("PM2.5:",
                           round(PAS$pm25[ind], 1),
                           "\U00B5g/m3"),
              "temp" = paste("Temperature:",
                             round(PAS$temperature[ind], 0),
                             "F"),
              "rh" = paste("Humidity:",
                           round(PAS$humidity[ind], 0),
                           "%"),
              "lng" = PAS$longitude[ind],
              "lat" =  PAS$latitude[ind]
            )

          # Make popup
          content <-
            paste(
              html$label,
              html$seen,
              html$pm,
              html$temp,
              html$rh,
              sep = "<br/>"
            )

          # Check which to draw on
          if( active$tab == "main" ) {
            proxy <- "leaflet"
            zoom <- input$leaflet_zoom
          }

          if( active$navtab == "latest" ) {
            proxy <- "latest_leaflet"
            zoom <- input$latest_leaflet_zoom
          }

          # Interact with proxy leaflet to avoid redraw
          leaflet::leafletProxy(proxy) %>%
            leaflet::clearPopups() %>%
            leaflet::addCircleMarkers(
              lng,
              lat,
              radius = 11,
              fillOpacity = 0,
              layerId = "selectTmp",
              options = leaflet::pathOptions(interactive = FALSE)
            ) %>%
            leaflet::addPopups(
              lng,
              lat,
              popup = content,
              options = leaflet::popupOptions(closeOnClick = FALSE, autoPan = TRUE))

        }

        # If on the comparison tab -> update the comparison leaflet
        if ( active$tab == "comp" ) {

          dates <- getDates()

          # Get data
          lab_pas <- active$label
          ind <- which(PAS$label == lab_pas)
          lng_pas <- PAS$longitude[ind]
          lat_pas <-  PAS$latitude[ind]

          # Interact with proxy comparison leaflet to avoid redraw
          leaflet::leafletProxy("shiny_leaflet_comparison") %>%
            leaflet::clearGroup("ws_markers") %>%
            leaflet::clearGroup("pas_markers") %>%
            # Selected pas markers
            leaflet::addCircleMarkers(
              lng = lng_pas,
              lat = lat_pas,
              radius = 11,
              fillOpacity = 0,
              group = "pas_markers",
              options = leaflet::pathOptions(interactive = FALSE),
              label = lab_pas,
              labelOptions = leaflet::labelOptions(
                noHide = TRUE,
                direction = "top"
              )
            )

          # Get nearest monitor ID
          nearestMonitor_Id <-
            shiny::isolate(active$pas$pwfsl_closestMonitorID)

          # Try loading nearest monitor
          nearestMonitor <-
            try(
              AirSensor::pwfsl_load(
                monitorIDs = nearestMonitor_Id,
                startdate = dates[1],
                enddate = dates[2]
              )
            )

          # If load is successful -> add monitor to map
          if ( !"try-error" %in% class(nearestMonitor) ) {

            lab_ws <-  nearestMonitor$meta$monitorID
            lat_ws <-  nearestMonitor$meta$latitude
            lng_ws <-  nearestMonitor$meta$longitude

            # Nearest monitor markers
            leaflet::leafletProxy("shiny_leaflet_comparison") %>%
              leaflet::addAwesomeMarkers(
                lng = lng_ws,
                lat = lat_ws,
                group = "ws_markers",
                label = lab_ws,
                labelOptions = leaflet::labelOptions(
                  noHide = TRUE,
                  direction = "bottom"
                ),
                icon = leaflet::makeAwesomeIcon(
                  icon="asterisk",
                  library="fa",
                  markerColor = "lightred",
                  iconColor = "black",
                  spin = T
                )

              )

            # If unsuccessful, add error label
          } else {

            leaflet::leafletProxy("shiny_leaflet_comparison") %>%
              leaflet::addAwesomeMarkers(
                lng = lng_pas,
                lat = lat_pas,
                group = "ws_markers",
                label = "Error: Cannot load nearest monitor location",
                labelOptions = leaflet::labelOptions(
                  noHide = TRUE,
                  direction = "bottom"
                )
              )

          }

        }

      }

    # (f)unction query the url
    fquery <-
      function() {
        shiny::reactive({
          # -- Query list based on url
          query <-
            shiny::parseQueryString(session$clientData$url_search)

          # -- Define updates based on query
          # Update community selection based on query
          shiny::updateSelectInput(
            session,
            inputId = "comm_select",
            selected = query[["communityId"]]
          )

          # Update selected tab based on query
          shiny::updateTabsetPanel(
            session,
            inputId = "tab_select",
            selected = query[["tb"]]
          )

          # Update selected pas based on query
          shiny::updateSelectInput(
            session,
            inputId = "pas_select",
            selected = query[["sensorId"]]
          )

          shiny::updateNavbarPage(
            session,
            inputId = "navtab",
            selected = query[["nav"]]
          )
        })

      }

    # Create a (n)ew query string
    nquery <-
      function() {

        # Community
        cComm <- active$communityId

        # Nav tab
        cNav <- active$navtab
        # Current tab
        cTab <- active$tab

        # Current pas select
        cPas <- active$label

        # -- Define queries to update based on input
        # Update the community string
        shiny::updateQueryString(
          paste0(
            "?nav=",
            cNav,
            "&",
            "?tb=",
            cTab,
            "&",
            "?communityId=",
            cComm,
            "&",
            "?sensorId=",
            cPas
          )
        )

      }

    helpText <-
      function() {

        shiny::req(active$tab)

        textdb <-
          c(
            "This is example help info."
          )

        if ( active$tab == "main" ) {
          txt <-
            shiny::HTML(
              "<small>
            <p>
            On this page, you can view all of the air quality sensors deployed through the
            US EPA funded STAR Grant at South Coast AQMD, entitled “Engage, Educate
            and Empower California Communities on the Use and Applications of
            Low-cost Air Monitoring Sensors”. The drop down menus above allow you to
            view individual participating communities or highlight individual sensors
            within the pre-selected community. You can control the timeframe shown in
            the bar plot by (1) choosing a date and (2) choosing a number of days to
            “look back”.
            </p>
            <p>
            The colors on the map illustrate the most recent hourly average PM2.5 value,
            for each site. The bar plot (below the map), shows the hourly average PM2.5
            values throughout the selected timeframe, for the selected site/sensor. The
            calendar plot (to the right of the map) shows the historic daily averages for
            the selected site/sensor. Note, the same color scale (in the bottom right)
            defines the colors in the map, bar chart, and calendar plot.
            </p>
            </small>"
            )
        } else if ( active$tab == "comp") {
          txt <-
            shiny::HTML(
              "<small>
            <p>
            Once you select a sensor, the map will display the location of the nearest regulatory
            monitoring station (or AirNow site), and the plots will provide a comparison of the data
            from that sensor and the nearest regulatory monitoring site. The time series provides a
            qualitative comparison of the hourly-averaged data, while the scatter plot provides some
            additional statistics to better assess this comparison. In terms of the data displayed in the
            scatter plot – on the horizontal axis (or x-axis) the data from the low-cost air quality sensor
            is plotted, while on the vertical axis (or y-axis) the data from the regulatory monitoring
            station (or AirNow site) is plotted.
            </p>
            <p>
            In terms of the statistics shared in the scatterplot, a slope close to 1.0 indicates that the
            low-cost sensor and the reference reflect similar levels. A slope greater than 1.0 indicates
            higher values are seen at the regulatory monitoring site and vice versa for a slope of less
            than 1.0. The intercept can be an indicator of bias, particularly if the slope is close to 1.0,
            but the intercept is either greater or less than zero. Finally, R2 provides an indication of how
            well the trends agree between the two sites; an R2 closer to 1.0 indicates more agreement
            and an R2 closer to 0.0 indicates less agreement.
            </p>
            </small>"
            )
        } else if ( active$tab == "dp" ) {
          txt <-
            shiny::HTML(
              "<small>
            <p>
            This tab illustrates the average daily trends for a single sensor using
            two different plots. The bar plot (on the left) lays out the average PM2.5
            values for each hour of the day. The “donut” plot (on the right),
            displays these averages in a circular pattern. Note, for both plots the
            grey shading indicates nighttime hours and the data used to calculate
            these averages is based on the time frame selected.
            </p>
            </small>"
            )
        } else if ( active$tab == "raw" ) {
          txt <-
            shiny::HTML(
              "<small>
            <p>
            Here you can view the “raw data” for the selected sensor, during the
            time frame selected. This data has undergone minimal QA/QC allowing
            you to view the highest time resolution PM2.5 data from Channel A,
            Channel B, and the temperature and humidity signals. Also included on
            this page are some additional meteorological data for the time period
            of interest and a pollution rose. The meteorological information can
            help you understand whether any events occurred that might impact
            the air quality or sensor performance. The pollution rose illustrates the
            direction the wind was blowing from when various pollutant
            concentrations were recorded by the selected sensor.
            </p>
            </small>"
            )
        } else if ( active$tab == "anim" ) {
          txt <-
            shiny::HTML(
              "<small>
            <p>
            On this page you can view animations of hourly average PM2.5 mass
            concentrations changing over time. Choose your community and a time
            frame and press ‘play’. Using the same color scale as other visualization
            in the app, purple corresponds to higher levels of PM2.5 and light blue
            to lower levels. The shading of the time slider indicates day vs. night,
            with white indicating daytime hours and grey indicating nighttime
            hours.
            </p>
            </small>"
            )
        }

        return(txt)

      }

    # ----- Reactive Observations ----------------------------------------------
    # NOTE: For use with low-hierarchy update functions and features.

    # Trigger active pat update based on pas selection, lookback, enddate, exp_label
    shiny::observeEvent(
      c( active$label,
         active$lookback,
         active$enddate,
         active$exp_label,
         active$latest_label ),
      {
        result <-
          try({
            active$pat <-
              AirSensor::pat_load(
                label = active$label,
                startdate = getDates()[1],
                enddate = getDates()[2],
                timezone = TIMEZONE
              )
          })
        if ( "try-error" %in% class(result) ) {
          notify(
            paste0(active$label, " Unavailiable"),
            paste0("Sensor last seen: ", tags$br(), active$pas$lastSeenDate),
            duration = 15
          )
        }
      }
    )

    # Trigger update selected pas on marker click
    shiny::observeEvent(
      active$marker,
      shiny::updateSelectInput(
        session,
        inputId = "pas_select",
        selected = active$marker
      )
    )

    # Trigger update selected pas on data explorer pas select
    shiny::observeEvent(
      active$exp_label,
      { active$label <- active$latest_label <- active$exp_label }
    )

    # Trigger update when latest pas is selected in latest view
    shiny::observeEvent(
      active$latest_label,
      { active$label <- active$exp_label <- active$latest_label }
    )

    # Global observations
    shiny::observe({

      # Watch the pas_select based on the current active label and community
      shiny::updateSelectInput(
        session,
        inputId = "pas_select",
        choices = getPasLabels(),
        selected = active$label

      )

      # Update the data explorer selection based on active label
      shiny::updateSelectInput(
        session,
        inputId = "de_pas_select",
        selected = active$label,
        choices = c("Select Sensor...",
                    PAS$label[!is.na(PAS$communityRegion) &
                                !grepl("(<?\\sB)$", PAS$label) &
                                PAS$DEVICE_LOCATIONTYPE != "inside"])
      )

      # Update latest pas with label
      shiny::updateSelectInput(
        session,
        inputId = "latest_pas_select",
        choices = getPasLabels(),
        selected = active$label
      )

      # Update the load button
      shiny::updateActionButton(
        session,
        inputId = "loadButton",
        label = paste0("Load Latest:", active$label)
      )

      # Watch the active variables to update the URL
      # nquery()

    })

    # Trigger leaflet update based on marker and pas selections, and tab change
    shiny::observeEvent(
      c(active$marker, active$tab, active$navtab),
      updateLeaf()
    )
    shiny::observeEvent(
      c(active$label, active$tab, active$navtab),
      updateLeaf(label = active$label)
    )

    shiny::observeEvent(
      active$tab,
      {
        if (active$tab == "anim") {
          shinyjs::disable("pas_select")
        } else {
          shinyjs::enable("pas_select")
        }
      }
    )

    # Update once if the selected pas is null i.e On Startup
    # -- Using a random selected pas ATM
    shiny::observeEvent(
      is.null(active$label), once = TRUE,
      {
        shiny::updateSelectInput(
          session,
          inputId = "pas_select",
          selected = sample(getPasLabels(), 1)
        )
      })

    # ----- Bookmark & Restore -------------------------------------------------

    # Exclude inputs for bookmarking
    shiny::setBookmarkExclude(
      c( "de_date_select_button",
         "de_help_select",
         "de_date_select",
         "de_lookback_select",
         "de_pas_select",
         "latest_help_select",
         "latest_comm_select",
         "latest_pas_select",
         "leaflet_zoom",
         "help_select",
         "leaflet_marker_mouseover",
         "loadButton",
         "leaflet_bounds",
         "leaflet_marker_mouseout",
         "date_select_button",
         "shinyjs-resettable-dySummary_plot",
         "leaflet_center",
         "dySummary_plot_date_window"
      )
    )

    # Callback on bookmark button click to save important states
    shiny::onBookmark( function(state) {

      state$values$pas_select <- active$label
      state$values$date_select <- active$enddate
      state$values$lookback_select <- active$lookback

    })

    # Callback on restore after load
    shiny::onRestored(function(state) {

      # Update the pas
      shiny::updateSelectInput(
        session,
        inputId = "pas_select",
        selected = state$values$pas_select
      )

      # Update date input (add a day for some reason)
      shinyWidgets::updateAirDateInput(
        session,
        inputId = "date_select",
        value = lubridate::ymd(state$values$date_select) + lubridate::days(1),
        clear = TRUE,
      )

      # Update lookback
      shinyWidgets::updateRadioGroupButtons(
        session,
        inputId = "lookback_select",
        selected = state$values$lookback_select
      )

      # Redundant but necessary?
      active$lookback <- state$values$lookback_select

      active$enddate <- state$values$date_select

    })

    # Update the url when bookmark button clicked
    shiny::onBookmarked(function(url) {
      shiny::updateQueryString(url)
      shiny::showBookmarkUrlModal(url)
    })

    # ----- Outputs ------------------------------------------------------------

    # DEBUG OUTPUT
    #output$debug <- renderTable(session$clientData$pixelratio)

    # L Col
    output$mini_table <- renderMiniTable()

    # - Overview Tab -
    output$leaflet <- renderLeaf()
    #output$summary_plot <- renderBarPlot(plotType = "hourly_plot")
    output$dySummary_plot <- renderDygraphSummary()
    output$cal_plot <- renderCalPlot()

    # - Comparison Tab -
    output$shiny_leaflet_comparison <- renderLeaf()
    output$comparison_table <- renderCompTable()
    output$ws_ext <- renderExtFit()
    output$ws_comp <- renderMonitorComp()

    # - Raw tab -
    output$rose_plot <- renderRose()
    output$raw_plot <- renderMultiplot(columns = 1)
    output$met_table <- renderMetTable()

    # - Animation tab -
    output$video_out <- renderVideo()

    # - Daily patterns tab -
    output$pattern_plot <- renderPatternPlot()

    # - Data Explorer -
    output$data_explorer <- renderDataExplorer()
    output$meta_explorer <- renderMetaExplorer()
    output$download_data <- downloadButton()

    # - Latest Data -
    #output$dygraph_plot <- renderDygraphPlot()
    output$aux_plot <- renderAuxPlot()
    output$latest_leaflet <- renderLeaf()

    # - Help text -
    output$help_text <- shiny::renderUI({ if (active$help) HTML(helpText()) })

  }

