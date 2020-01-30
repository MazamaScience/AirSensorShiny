# ----- AirShiny Server Logic --------------------------------------------------

server <-
  function(input, output, session) {

    # Show memory usage on startup
    memory_debug("Start")

    # ----- Reactive Controls --------------------------------------------------

    # Define active user selections
    # NOTE: This contains all active data to avoid redundant func and load.
    #       Update the active values only on trigger events.
    active <- shiny::reactiveValues( pas = NULL,
                                     pat = NULL,
                                     label = NULL,
                                     enddate = NULL,
                                     community = NULL,
                                     lookback = NULL,
                                     marker = NULL,
                                     compmarker = NULL,
                                     tab = NULL,
                                     navtab = NULL,
                                     de_label = NULL,
                                     latest_load = NULL,
                                     latest_community = NULL,
                                     latest_label = NULL,
                                     communityId = NULL,
                                     help = NULL,
                                     worldmet = NULL,
                                     sensor = NULL )

    # Update the active variable with an input variable
    updateActive <-
      function(update, with) active[[update]] <- input[[with]][1]

    # Update active pas and label & Update active sensor
    shiny::observeEvent(
      label = "pas update",
      input$pas_select,
      ignoreInit = TRUE,
      {
        updateActive("label", "pas_select")
        active$pas <- PAS[grepl(active$label, PAS$label),][1,]

        active$sensor <-
          PWFSLSmoke::monitor_subset(
            ws_monitor = SENSORS,
            monitorIDs = active$label,
            timezone = TIMEZONE
          )
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

    # Update active lookback days
    shiny::observeEvent(
      label = "lookback update",
      input$lookback_select,
      updateActive("lookback", "lookback_select")
    )

    # Update active enddate
    shiny::observeEvent(
      label = "date update",
      input$date_select,
      {
        active$enddate <-
          input$date_select
        active$startdate <-
          lubridate::ymd(active$enddate) - lubridate::days(active$lookback)
      }
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
      updateActive("de_label", "de_pas_select")
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

        # sd <- lubridate::ymd(active$startdate, tz = TIMEZONE)

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

          sensors <- SENSORS$meta$monitorID

        } else {

          sensors <-
            SENSORS$meta$monitorID[
              grepl(
                community,
                SENSORS$meta$communityRegion
              )
            ]
        }

        return(sensors)

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
          labels <- SENSORS$meta$monitorID
        } else if ( grepl("Richmond|West Los Angeles|Oakland", community) ) {
          # HACKY: fix to deal with requested communtiy names
          tmp_com <- if (community=="Richmond") "SCAN" else if (community=="Oakland") "SCAH" else "SCUV"
          labels <- SENSORS$meta$monitorID[grepl(tmp_com, SENSORS$meta$communityRegion)]
        } else {
          labels <- SENSORS$meta$monitorID[grepl(community, SENSORS$meta$communityRegion)]
        }

        sensors <-
          PWFSLSmoke::monitor_subset(
            ws_monitor = SENSORS,
            monitorIDs = labels
          )

        return(sensors)

      }, label = "get community sensors")

    # Load latest data of the past day from thingspeak on load button press
    thingspeakLatest <-
      shiny::eventReactive(
        input$loadButton,
        {
          shiny::req(active$latest_label)

          ed <- lubridate::now(tzone = TIMEZONE)
          sd <- ed - lubridate::ddays(2)

          # Get both channels from global pas
          pas <- PAS[grepl(active$latest_label, PAS$label),]
          latest <-
            AirSensor::pat_createNew(
              pas = pas,
              label = active$latest_label,
              startdate = sd,
              enddate = ed
            )

          # Update page title
          output$latest_pageTitle <-
            shiny::renderUI({tags$h4("Latest ", active$latest_label, " Data")})

          return(latest)

        }, label = "load latest data from thingspeak")

    # ----- Render Handling ----------------------------------------------------

    # Render Map
    renderLeaf <-
      function() {
        leaflet::renderLeaflet({

          sensors <- getCommunitySensors()
          dates <- getDates()

          leaf <-
            shiny_sensorLeaflet(
              sensor = sensors,
              startdate = dates[1],
              enddate = dates[2],
              maptype = "Stamen.TonerLite",
              pat = active$pat
            )



          return(leaf)

        })
      }

    renderBarPlotly <-
      function() {
        plotly::renderPlotly({

          shiny::req(active$pat)

          dates <- getDates()
          # NOTE: Use the active$pat instead of the active$sensor to avoid year
          # NOTE: issues with autoloaded SENSORS
          tmp_sensor <- AirSensor::pat_createAirSensor(active$pat)

          bp <- tryCatch(expr = {shiny_barplotly(tmp_sensor, dates[1], dates[2])},
                         error = function(e) {
                           handleError(FALSE, "Summary plot failed. Please select a different sensor or date(s).")
                         }
          )

          return(bp)
        })
      }

    renderCalendar <-
      function() {
        plotly::renderPlotly({
          # shiny::req(active$sensor)
          dates <- getDates()
          # NOTE: Improve by implementing annual Sensor
          tmp <- AirSensor::pat_load( label = active$pas$label,
                                      startdate = paste0(lubridate::year(dates[1]), "0101"),
                                      enddate = paste0(lubridate::year(dates[2]), "1231") )
          handleError(AirSensor::pat_isPat(tmp) | AirSensor::pat_isEmpty(tmp),
                      "Calendar failed. Please select a different sensor.")
          pp_cal <- shiny_calendarPlot(tmp)
          return(pp_cal)
        })
      }

    # Render Selected mini table
    renderMiniTable <-
      function() {
        shiny::renderTable({

          if ( is.null(active$label) ||
               active$label == "Select Sensor..." ||
               active$label == "" ) {

            mini <-
              dplyr::tibble(
                "Latitude" ="",
                "Longitude" = ""
              )

          } else {

            mini <-
              dplyr::tibble(
                "Latitude" = active$pas$latitude,
                "Longitude" = active$pas$longitude
              )

          }

          return(mini)

        }, align = "c", spacing = "xs")
      }

    # Render the monitor comparison plot
    renderMonitorComp <-
      function() {
        shiny::renderPlot({
          req(active$pat)

          logger.trace("label = %s, pwfsl_closestMonitorID = %s",
                       active$pat$meta$label,
                       active$pat$meta$pwfsl_closestMonitorID)
          tlim <- range(active$pat$data$datetime)

          logger.trace("trange = c(%s, %s)",
                       strftime(tlim[1], tz = "UTC"),
                       strftime(tlim[2], tz = "UTC"))

          # NOTE: ggplot2 does not seem to play well with Shiny and tryCatch
          compPlot <- AirSensor::pat_monitorComparison(active$pat)

          handleError(
            ggplot2::is.ggplot(compPlot),
            "Comparison plot failed. Please try a different sensor."
          )

          logger.trace(geterrmessage())
          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please select a sensor to compare with the nearest monitor."
          )


          return(compPlot)

        })#, cacheKeyExpr = list(active$label, active$lookback, active$enddate))
      }

    # Render monitor & pas external fit plot
    renderExtFit <-
      function() {
        shiny::renderCachedPlot({
          req(active$sensor, active$label)
          date <- getDates()

          extPlot <- shiny_externalFit( sensor = active$sensor,
                                        startdate = date[1],
                                        enddate = date[2] )

          return(extPlot)

        }, cacheKeyExpr = list(active$label, active$lookback, active$enddate))
      }

    # Render Multiplot
    renderMultiplot <-
      function(columns = NULL) {
        shiny::renderCachedPlot({
          shiny::req(active$pat)

          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please select a sensor."
          )

          if ( is.null(columns) ) {
            columns <- 2
          }

          multiPlot <- AirSensor::pat_multiplot(active$pat, columns = columns)

          return(multiPlot)

        }, cacheKeyExpr = list(active$label, active$lookback, active$enddate))
      }

    # Render rose plot
    renderRose <-
      function() {
        shiny::renderCachedPlot({
          shiny::req(active$label)

          dates <- getDates()
          # load sensor
          sensor <- AirSensor::sensor_load(
            startdate = dates[1],
            enddate = dates[2]
          )

          logger.trace("sensor_load(%s, %s) returns %d rows of data",
                       strftime(dates[1]),
                       strftime(dates[2]),
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

          # Add active worldmet for use later
          active$worldmet <- metData

          # filter wind data
          windData <- dplyr::select(metData, c("date", "wd", "ws"))

          result <-
            try({
              rose <- AirSensor::sensor_pollutionRose(sensor, windData)
            }, silent = FALSE)

          if ( "try-error" %in% class(result) ) {
            logger.trace(geterrmessage())
            notify("Rose Plot Failed")
            handleError("", paste0(active$label, ": Rose Plot Unavailable"))
          }

          logger.trace("class(rose) = %s", class(rose))

          return(rose)

          memory_debug("Rose Plot")

        }, cacheKeyExpr = list(active$label, active$lookback, active$enddate))
      }

    # Render video
    renderVideo <-
      function() {
        shiny::renderUI({
          shiny::req(active$enddate, active$community, active$communityId)

          baseUrl <- "http://smoke.mazamascience.com/data/PurpleAir/videos/"
          year    <- strftime(active$enddate, "%Y", tz = TIMEZONE)
          mm      <- strftime(active$enddate, "%m", tz = TIMEZONE)
          dd      <- strftime(active$enddate, "%d", tz = TIMEZONE)
          comm    <- active$communityId

          url <- paste0(baseUrl, year, "/", comm, "_", year, mm, dd, ".mp4" )

          tags$video(
            id = "video",
            type = "video/mp4",
            src = url,
            controls = "controls"
          )

        })
      }

    # Render Meta table explorer
    renderMetaExplorer <-
      function() {
        shiny::renderTable({
          req(active$pas, active$label)

          pat <- active$pat
          community <- active$pat$meta$communityRegion

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
          req(active$pat)
          memory_debug("Data Explorer")

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
        shiny::renderCachedPlot({
          req(active$pat)

          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please select a sensor."
          )

          dates <- getDates()
          patternPlot <-
            shiny_diurnalPattern(
              sensor = active$sensor,
              startdate = dates[1],
              enddate = dates[2]
            )

          return(patternPlot)

        }, cacheKeyExpr = list(active$label, active$lookback, active$enddate))
      }

    renderDygraphPlot <-
      function() {
        dygraphs::renderDygraph({

          dygraph <-
            AirSensor::pat_dygraph(
              pat = active$pat,
              sampleSize = NULL
            )

          return(dygraph)

        })
      }

    renderLatestPlot <-
      function() {
        shiny::renderPlot({

          latest <- thingspeakLatest()

          auxPlot <-
            AirSensor::pat_multiplot(
              pat = latest,
              plottype = "all", #"aux",
              sampleSize = NULL,
              columns = 1
            )

          return(auxPlot)

        })
      }

    renderCompTable <-
      function() {
        DT::renderDataTable({
          shiny::req(active$pat)

          compTable <-
            shiny_comparisonTable(active$pat) %>%
            DT::datatable(
              selection = "none",
              colnames = "",
              options = list(dom = 't', bSort = FALSE),
              class = 'cell-border stripe'
            ) %>%
            DT::formatRound(columns = 1, digits = 2)

          return(compTable)

        })
      }

    renderMetTable <-
      function() {
        DT::renderDataTable({
          shiny::req(active$pat, active$label,active$worldmet)

          metData <- active$worldmet
          table <-
            shiny_metTable(metData) %>%
            DT::datatable(
              selection = "none",
              colnames = "",
              options = list(dom = 't', bSort = FALSE),
              class = 'cell-border stripe'
            ) %>%
            DT::formatRound(columns = 1, digits = 2)

          return(table)

        })
      }

    renderDygraphSummary <-
      function() {
        dygraphs::renderDygraph({
          shiny::req(active$sensor)

          shinyjs::reset("dySummary_plot")

          dates <- getDates()
          result <-
            try({
              dySummary <-
                shiny_dySummary(
                  sensor = active$sensor,
                  startdate = dates[1],
                  enddate = dates[2]
                )
            }, silent = TRUE)

          if ( "try-error" %in% class(result) ) {
            logger.trace(geterrmessage())
            notify("Summary Failed")
            handleError("", paste0(active$label, ": Failed"))
          }

          return(dySummary)

        })
      }

    renderPatternText <-
      function() {
        shiny::renderText({

          ed <- active$enddate
          sd <- lubridate::ymd(active$enddate, tz = TIMEZONE) -
            lubridate::days(active$lookback)

          return(paste0("From ", sd, " to ", ed))

        })
      }

    renderOverlayPlot <-
      function() {
        shiny::renderCachedPlot({
          shiny::req(active$pat, active$label)

          result <-
            try({
              abPlot <-
                shiny_internalFit(pat = active$pat, whichPlot = "ab")
            })

          if ( "try-error" %in% class(result) ) {
            logger.trace(geterrmessage())
            handleError("", "")
          }

          return(abPlot)

        }, cacheKeyExpr = list(active$label, active$lookback, active$enddate))
      }

    renderLMPlot <-
      function() {
        shiny::renderCachedPlot({
          shiny::req(active$pat, active$label)

          result <-
            try({ lmPlot <-
              shiny_internalFit(pat = active$pat, whichPlot = "lm") })

          if ( "try-error" %in% class(result) ) {
            logger.trace(geterrmessage())
            handleError("", "")
          }

          return(lmPlot)

        }, cacheKeyExpr = list(active$label, active$lookback, active$enddate))
      }

    renderHelpless <-
      function() {
        shiny::renderUI({

          if (active$help) {
            shiny::wellPanel(
              HTML(helpText())
            )
          }

        })
      }


    # ----- Helper functions ---------------------------------------------------

    # Overwrite the global sensors list for different years
    loadAnnualSensors <-
      function(startdate = NULL) {
        SENSORS <-
          AirSensor::sensor_loadYear( collection = "scaqmd",
                                      datestamp = lubridate::year(startdate) )
        return(SENSORS)
      }

    # Handle download button
    downloadButton <-
      function() {
        shiny::downloadHandler(
          filename = function() {
            dates <- getDates()
            paste0(active$pat$meta$label, "_", dates[1], "_", dates[2], ".csv")
          },
          content = function(file) {
            pat <- active$pat
            write.csv(pat$data[1:5], file = file)
          }
        )
      }

    # Update leaflet function for selected pas, or map click
    updateLeaf <-
      function(label = NULL) {
        # If on main tab -> update main leaflet
        if ( active$tab == "main" || active$navtab == "latest") {
          # Get data
          if ( is.null(label) ) {
            label <- active$marker
          }

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
          if ( active$tab == "main" ) {
            proxy <- "leaflet"
            zoom <- input$leaflet_zoom
          }

          if ( active$navtab == "latest" ) {
            proxy <- "latest_leaflet"
            zoom <- input$latest_leaflet_zoom
          }

          # Interact with proxy leaflet to avoid redraw
          leaflet::leafletProxy(proxy) %>%
            leaflet::clearPopups() %>%
            leaflet::addCircleMarkers(
              lng,
              lat,
              radius = 10,
              fillOpacity = 0.95,
              layerId = "selectTmp",
              color = "#ffa020",
              options = list(leaflet::pathOptions(interactive = FALSE))
            ) %>%
            leaflet::addPopups(
              lng,
              lat,
              popup = content,
              options = leaflet::popupOptions(closeOnClick=FALSE, autoPan=TRUE)
            )

        }

        # If on the comparison tab -> update the comparison leaflet
        if ( active$tab == "comp" ) {

          shiny::req(active$label)

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
              radius = 10,
              fillOpacity = 0.95,
              group = "pas_markers",
              options = leaflet::pathOptions(interactive = FALSE),
              label = lab_pas,
              color = "#ffa020",
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

            lab_ws <-  nearestMonitor$meta$siteName
            lat_ws <-  nearestMonitor$meta$latitude
            lng_ws <-  nearestMonitor$meta$longitude

            # Nearest monitor markers
            leaflet::leafletProxy("shiny_leaflet_comparison") %>%
              leaflet::addAwesomeMarkers(
                lng = lng_ws,
                lat = lat_ws,
                group = "ws_markers",
                label = HTML("<b> Nearest Monitor Site </b> <br>",lab_ws),
                labelOptions = leaflet::labelOptions(
                  noHide = TRUE,
                  direction = "bottom"
                ),
                icon = leaflet::makeAwesomeIcon(
                  icon = "dot-circle",
                  library = "fa",
                  markerColor = "orange",
                  iconColor = "black"
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

    helpText <-
      function() {
        shiny::req(active$tab)

        if ( active$tab == "main" ) {
          txt <- main_helpText
        } else if ( active$tab == "comp") {
          txt <- comparison_helpText
        } else if ( active$tab == "dp" ) {
          txt <- dailyPatterns_helpText
        } else if ( active$tab == "raw" ) {
          txt <- raw_helpText
        } else if ( active$tab == "anim" ) {
          txt <- animation_helpText
        } else if ( active$tab == "calendar" ) {
          txt <- calendar_helpText
        }

        return(txt)

      }

    # ----- Reactive Observations ----------------------------------------------
    # NOTE: For use with low-hierarchy update functions and features.

    # --- LOAD PAT -- VERY IMPORTANT ---
    shiny::observeEvent(
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      c( active$label,
         active$lookback,
         active$enddate,
         active$de_label,
         active$latest_label ),
      {
        shiny::req(active$label)
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
        memory_debug("PAT LOAD")
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
      active$de_label,
      { active$label <- active$latest_label <- active$de_label }
    )

    # Trigger update when latest pas is selected in latest view
    shiny::observeEvent(
      active$latest_label,
      { active$label <- active$de_label <- active$latest_label }
    )

    # shiny::observeEvent(
    #   active$enddate,
    #   {
    #     # Update sensors on year change
    #     if ( lubridate::year(active$enddate) != lubridate::year(active$sensor$data$datetime[1]) ) {
    #       SENSORS <<- loadAnnualSensors(startdate = active$enddate)
    #       active$sensor <- PWFSLSmoke::monitor_subset(ws_monitor = SENSORS, monitorIDs = active$label)
    #     }
    #   }
    # )

    # Global observations
    shiny::observe({

      # Determine which pas selection to update
      if ( active$navtab == "latest" ) {
        pasInput <- "latest_pas_select"
      } else if ( active$navtab == "dataview" ) {
        pasInput <- "de_pas_select"
      } else {
        pasInput <- "pas_select"
      }

      shiny::updateSelectInput(
        session,
        inputId = pasInput,
        choices = getPasLabels(),
        selected = active$label
      )

      # Popup notifcation if sensor is not selected
      if ( active$label == "" ) {
        if ( active$tab != "main" & active$tab != "anim") {
          shinyWidgets::sendSweetAlert(
            session,
            title = "Please Select a Sensor",
            type = "warning",
            closeOnClickOutside = TRUE
          )
        }
        shinyjs::hide(id = "summary_barplot", anim = FALSE)
      } else {
        shinyjs::show(id = "summary_barplot", anim = TRUE)
        shinyjs::hide(id = "sensorIsSelected", anim = FALSE)
      }

      # Only allow downloadable data when a label is selected
      if ( active$de_label == "" ) {
        shinyjs::disable(id = "download_data")
      } else {
        shinyjs::enable(id = "download_data")
      }

      # Only show message in Video tab if community is not selected
      if ( active$community  == "" |
           active$community == "all" ) {
        shinyjs::show(id = "communityIsSelected")
      } else {
        shinyjs::hide(id = "communityIsSelected")
      }

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

    # disable box when not needed on community tab
    shiny::observeEvent(
      active$tab,
      {
        if (active$tab == "anim") {
          shinyjs::hide("pas_select", anim = TRUE)
          shinyjs::hide("lookback_select", anim = TRUE)
        } else {
          shinyjs::show("pas_select", anim = TRUE)
          shinyjs::show("lookback_select", anim = TRUE)
        }
      }
    )

    # Help text toggle
    shiny::observeEvent(
      active$help,
      if ( active$help ) {
        shinyjs::show("help_text", anim = TRUE)
      } else {
        shinyjs::hide("help_text", anim = TRUE)
      }
    )

    # Update the the labels when switching tabs
    shiny::observeEvent(
      active$navtab,
      {
        shiny::updateSelectInput(
          session,
          inputId = "pas_select",
          selected = active$label
        )

        shiny::updateSelectInput(
          session,
          inputId = "de_pas_select",
          selected = active$label
        )

        shiny::updateSelectInput(
          session,
          inputId = "latest_pas_select",
          selected = active$label
        )
      }
    )

    # ----- Bookmark & Restore -------------------------------------------------

    # Observe bookmarking button
    shiny::observeEvent(
      c(input$de_bookmark, input$exp_bookmark),
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        session$doBookmark()
      }
    )

    # Callback on bookmark button click to save important states
    shiny::onBookmark(
      fun = function(state) {
        state$values$pas_select <- active$label
        state$values$date_select <- active$enddate
        state$values$lookback_select <- active$lookback
        state$values$tab <- active$tab
        state$values$nav <- active$navtab
      },
      session = session
    )

    # Callback on restore after load
    shiny::onRestored(
      fun = function(state) {
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
          value = lubridate::ymd(state$values$date_select, tz = TIMEZONE) +
            lubridate::days(1),
          clear = TRUE,
        )
        # Update lookback
        shinyWidgets::updateRadioGroupButtons(
          session,
          inputId = "lookback_select",
          selected = state$values$lookback_select
        )
        # Update nav tab
        shiny::updateNavbarPage(
          session,
          inputId = "navtab",
          selected = state$values$nav
        )
        # Update tab select
        shiny::updateTabsetPanel(
          session,
          inputId = "tab_select",
          selected = state$values$tab
        )
        # Redundant but necessary?
        active$lookback <- state$values$lookback_select
        active$enddate <- state$values$date_select
      },
      session = session
    )

    # Update the url when bookmark button clicked
    shiny::onBookmarked(
      fun = function(url) {
        url <- paste0( stringr::str_match(url, "http:(.+)/\\/?")[1],
                       "?",
                       stringr::str_match(url, "_values_(.*)")[1] )
        shiny::updateQueryString(url)
        shiny::showBookmarkUrlModal(url)
      },
      session = session
    )

    # ----- On Flush -----------------------------------------------------------
    shiny::onStop(
      function() {
        memory_debug("ON EXIT")
        gc(reset = TRUE)
      }
    )

    # ----- Outputs ------------------------------------------------------------

    # - Overview Tab -
    output$leaflet <- renderLeaf()
    # output$dySummary_plot <- renderDygraphSummary()
    output$summary_barplot <- renderBarPlotly()
    output$sensorIsSelected <- shiny::renderUI({"Please Select a Sensor."})

    # - Calendar Tab -
    output$calendar_plot <- renderCalendar()

    # - Comparison Tab -
    output$shiny_leaflet_comparison <- renderLeaf()
    output$comparison_table <- renderCompTable()
    output$ws_ext <- renderExtFit()
    output$ws_comp <- renderMonitorComp()
    output$pattern_title <- renderPatternText()

    # - Raw tab -
    output$rose_plot <- renderRose()
    output$raw_plot <- renderMultiplot(columns = 1)
    output$met_table <- renderMetTable()
    output$ab_comp_plot <- renderOverlayPlot()
    output$lm_comp_plot <- renderLMPlot()

    # - Animation tab -
    output$video_out <- renderVideo()
    output$communityIsSelected <- shiny::renderUI({"Please Select a Community."})

    # - Daily patterns tab -
    output$pattern_plot <- renderPatternPlot()

    # - Data Explorer -
    output$data_explorer <- renderDataExplorer()
    output$meta_explorer <- renderMetaExplorer()
    output$download_data <- downloadButton()

    # - Latest Data -
    output$latest_plot <- renderLatestPlot()

    # - Help text -
    output$help_text <- renderHelpless()


  }
