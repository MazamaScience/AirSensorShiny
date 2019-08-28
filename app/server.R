# ----- AirShiny Server Logic --------------------------------------------------

shiny::shinyServer(
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

        if ( active$navtab == "latest" ) {
          community <- active$latest_community
        } else {
          community <- active$community
        }

        if ( community == "all" )  {
          pas <- PAS[which(!is.na(PAS$communityRegion)),]
        } else {
          pas <-
            PAS[which(
              stringr::str_detect(
                PAS$communityRegion,
                community)
            ),]
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

            shiny_barplot(
              pat,
              period = "1 day",
              startdate = dates[1],
              enddate = dates[2]
            )

          }  else if ( plotType == "hourly_plot" ) {

            shiny_barplot(
              pat,
              period = "1 hour",
              startdate = dates[1],
              enddate = dates[2]
            )

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
          ed <- active$enddate
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
          # Hour (HH) disabled
          # hh <- "09"
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
              "Sensor Type" = active$pas$sensorType, #pat$meta$sensorType,
              "Longitude" = active$pas$longitude,#pat$meta$longitude,
              "Latitude" = active$pas$latitude, #pat$meta$latitude,
              "State" = active$pas$stateCode,#pat$meta$stateCode,
              "Country" = active$pas$countryCode, #pat$meta$countryCode,
              "Timezone" = active$pas$timezone#pat$meta$timezone
            )

          return(meta)

        })

      }

    # Render Data Table Explorer
    renderDataExplorer <-
      function() {

        shiny::renderDataTable({

          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please select a sensor."
          )

          req(active$pat)

          showLoad(shiny::incProgress(0.7))

          # Remove unecessary columns
          data <- active$pat$data[-(6:10)]

          names(data) <- c("Datetime", "PM2.5 Ch. A (\u03bcg / m\u00b)","PM2.5 Ch. B (\u03bcg / m\u00b)", "Temperature (F)", "Relative Humidity (%)")

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

            shiny_diurnalPattern(active$pat) #+ ggplot2::scale_fill_brewer()


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

          shiny_metTable(metData)
        })
        }, bordered = TRUE, align = "c")

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

    # Load the pat into the active pat (reactive expr only)
    loadPat <-
      function() {

        if ( active$navtab == "explore" ) label <- active$label
        if ( active$navtab == "dataview" ) label <- active$exp_label
        if ( active$navtab == "latest" ) label <- active$latest_label

        dates <- getDates()
        active$pat <-
          try(
            AirSensor::pat_load(
              label,
              dates[1],
              dates[2]
            )
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
          if( active$tab == "main" ) proxy <- "leaflet"; zoom <- input$leaflet_zoom
          if( active$navtab == "latest" ) proxy <- "latest_leaflet"; zoom <- input$latest_leaflet_zoom

          # Interact with proxy leaflet to avoid redraw
          leaflet::leafletProxy(proxy) %>%
            leaflet::clearPopups() %>%
            # leaflet::setView(#flyTo(
            #   lng,
            #   lat,
            #   zoom = ifelse(is.null(input$leaflet_zoom), 12, input$leaflet_zoom),
            #   options = list(animate=FALSE)
            # ) %>%
            # Add a selected PAS marker
            # NOTE: Marker given tmp layerId for hacky temp
            # visual workaround. Interactive is false for hacky
            # workaround to "send to back" to avoid popup conflict.
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
            # leaflet::flyTo(
            #   lng_pas,
            #   lat_pas,
            #   zoom = 12,
            #   option = list(animate=FALSE) #Problem
            # ) %>%
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

      }

    # Create a (n)ew query string
    nquery <-
      function() {

        # Community
        # -- Substitute spaces if true
        cComm <- active$communityId
          # ifelse(
          #   grepl("\\s", active$community ),
          #   gsub("\\s","\\+", active$community),
          #   active$community
          # )
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

        textdb <-
          c(
            "This is example help info."
          )

        ### EXAMPLE

        return(HTML("Example Help Text: ", sample(textdb, 1)))


      }

    # ----- Reactive Observations ----------------------------------------------
    # NOTE: For use with low-hierarchy update functions and features.

    # Trigger active pat update based on pas selection, lookback, enddate, exp_label
    shiny::observeEvent(
      c(active$label, active$lookback, active$enddate, active$exp_label, active$latest_label),
      loadPat()
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
        choices = c("Select Sensor...", getPasLabels()),
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
      nquery()

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

    # ----- Outputs ------------------------------------------------------------

    # DEBUG OUTPUT
    #output$debug <- renderTable(session$clientData$pixelratio)

    # L Col
    output$mini_table <- renderMiniTable()

    # - Overview Tab -
    output$leaflet <- renderLeaf()
    output$summary_plot <- renderBarPlot(plotType = "hourly_plot")
    output$cal_plot <- renderCalPlot()

    # - Comparison Tab -
    output$shiny_leaflet_comparison <- renderLeaf()
    output$comparison_table <- renderCompTable()
    output$ws_ext <- renderExtFit()
    output$ws_comp <- renderMonitorComp()

    # - Raw tab -
    output$rose_plot <- renderRose()
    output$raw_plot <- renderMultiplot(columns = 4)
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
    output$help_text <- shiny::renderText({ if (active$help) helpText()})


  }

)

#  ----- CURRENT ISSUES: -----

# - URL querying/Bookmarking

