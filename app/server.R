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
        dexp = NULL,
        latestload = NULL
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
      updateActive("community", "comm_select")
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
      input$comp_leaflet_marker_click,
      updateActive("marker", "comp_leaflet_marker_click")
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
      updateActive("dexp", "de_pas_select")
    )

    # Update active marker with latest leaflet marker click
    shiny::observeEvent(
      label = "latest map marker update",
      input$latest_leaflet_marker_click,
      updateActive("marker", "latest_leaflet_marker_click")
    )

    # ----- Reactive Functions -------------------------------------------------

    # Get the dates
    getDates <-
      shiny::reactive({

        sd <- lubridate::ymd(active$enddate) -
          lubridate::ddays(as.numeric(active$lookback))

        ed <- lubridate::ymd(active$enddate)

        return(c(sd, ed))

      }, label = "get dates")

    # Get the valid pas selectables
    getPasLabels <-
      shiny::reactive({

        # NOTE: Remove any pas with no def community & contains B & inside
        if ( active$community == "all" ) {
          return(PAS$label[!is.na(PAS$communityRegion) &
                             !grepl("(<?\\sB)$", PAS$label) &
                             PAS$DEVICE_LOCATIONTYPE != "inside"])
        } else {
          return(PAS$label[grepl(active$community, PAS$communityRegion) &
                             !grepl("(<?\\sB)$", PAS$label) &
                             PAS$DEVICE_LOCATIONTYPE != "inside"])
        }

      }, label = "get pas labels")

    # Get the sensors in the selected community
    getCommunitySensors <-
      shiny::reactive({

        if ( active$community == "all" )  {

          pas <- PAS[which(!is.na(PAS$communityRegion)),]

        } else {

          pas <-
            PAS[which(
              stringr::str_detect(
                PAS$communityRegion,
                active$community)
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
          sd <- lubridate::ymd(Sys.Date())

          ed <- lubridate::ymd(Sys.Date()) + lubridate::ddays(1)

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

          utils_leaflet(
            pas = valid_sensors,
            parameter = "pm25_current",
            paletteName = "Spectral" #"Purple"
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

            utils_barplot(
              pat,
              period = "1 day",
              startdate = dates[1],
              enddate = dates[2]
            )

          }  else if ( plotType == "hourly_plot" ) {

            utils_barplot(
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

          # Create start of year date
          sd <- paste0(strftime(active$enddate, "%Y"), "0101")

          showLoad({

            # Load annual pat to now
            pat <-
              try(
                AirSensor::pat_load(
                  active$label,
                  startdate = sd,
                  enddate = Sys.Date()
                )
              )

            if ( class(pat) == "try-error" ) {
              handleError("", "Error: Please select a different sensor.")
              shiny::showNotification("HEY")
            }

            shiny::incProgress(0.65)

            # Calendar plot
            calendar <- try(AirSensor::pat_calendarPlot(pat))

            if ( class(calendar)  == "try-error" ) {

              shiny::showNotification(
                type = "warning",
                HTML("<b> Calendar Creation Failed</b> <br>
                     Please select a different sensor.")
                )

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

          req(active$label, active$pas)

          dplyr::tibble(
            "Sensor" = active$label,
            "Latitude" = active$pas$latitude,
            "Longitude" = active$pas$longitude
          )

        })

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

          utils_externalFit(active$pat)

        })

      }

    # Render Multiplot
    renderMultiplot <-
      function() {

        shiny::renderPlot({

          handleError(
            AirSensor::pat_isPat(active$pat),
            "Please select a sensor."
          )

          shiny::req(active$pat)

          AirSensor::pat_multiplot(active$pat)

        })

      }

    # Render rose plot
    renderRose <-
      function() {

        shiny::renderPlot({

          shiny::req(active$label)

          showLoad({

            dates <- getDates()

            sensor <-
              AirSensor::sensor_filterMeta(
                monitorID == active$label,
                sensor = AirSensor::sensor_load(
                  startdate = dates[1],
                  enddate = dates[2]
                )
              )

            shiny::incProgress(0.44)

            AirSensor::sensor_pollutionRose(sensor)

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
            strftime(active$enddate, "%Y")
          mm <-
            strftime(active$enddate, "%m")
          dd <-
            strftime(active$enddate, "%d")
          hh <- "09"
          comm <- active$community

          url <-
            paste0(
              baseUrl,
              year,
              "/",
              comm,
              "_",
              year,
              mm,
              dd,
              hh,
              ".mp4"
            )


          tags$video(
            id="video2",
            type = "video/webm",
            src = url,
            controls = "controls", loop = TRUE
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

          return(active$pat$data)

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

            utils_patternPlot(active$pat)

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
        if ( active$navtab == "dataview" ) label <- active$dexp

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
          if( active$tab == "main" ) proxy <- "leaflet"
          if( active$navtab == "latest" ) proxy <- "latest_leaflet"

          # Interact with proxy leaflet to avoid redraw
          leaflet::leafletProxy(proxy) %>%
            leaflet::clearPopups() %>%
            leaflet::flyTo(
              lng,
              lat,
              zoom = input$leaflet_zoom
            ) %>%
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
            leaflet::addPopups(lng, lat, popup = content)

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
          leaflet::leafletProxy("comp_leaflet") %>%
            leaflet::clearGroup("ws_markers") %>%
            leaflet::clearGroup("pas_markers") %>%
            leaflet::flyTo(
              lng_pas,
              lat_pas,
              zoom = input$comp_leaflet_zoom #Problem
            ) %>%
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
          if ( class(nearestMonitor) != "try-error" ) {

            lab_ws <-  nearestMonitor$meta$monitorID
            lat_ws <-  nearestMonitor$meta$latitude
            lng_ws <-  nearestMonitor$meta$longitude

            # Nearest monitor markers
            leaflet::leafletProxy("comp_leaflet") %>%
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

            leaflet::leafletProxy("comp_leaflet") %>%
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
        cComm <-
          ifelse(
            grepl("\\s", active$community ),
            gsub("\\s","\\+", active$community),
            active$community
          )
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
            "See the skipping of the man,
            I think he's angry at the lifespan.",

            "He finds it hard to see the deer,
            Overshadowed by the charming yesteryear.",

            "Who is that waddling near the eagle?
            I think she'd like to eat the illegal.",

            "Her goofy car is just a cheese,
            It needs no gas, it runs on munchies.",

            "She's not alone she brings a camel,
            a pet octopus, and lots of nail enamel.",

            "The octopus likes to chase a salt,
            Especially one that's in the renault.",

            "The man shudders at the amazing deer
            He want to leave but she wants the stratosphere."
          )

        ### EXAMPLE

        return(
          HTML(
            "EXAMPLE HELP TEXT: ",
            sample(textdb, 1)
          )
          )


      }

    # ----- Reactive Observations ----------------------------------------------
    # NOTE: For use with low-hierarchy update functions and features.

    # Trigger active pat update based on pas selection, lookback, enddate, dexp
    shiny::observeEvent(
      c(active$label, active$lookback, active$enddate, active$dexp),
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

    shiny::observeEvent(
      active$marker,
      shiny::updateSelectInput(
        session,
        inputId = "latest_pas_select",
        selected = active$marker
      )
    )

    # Trigger update selected pas on data explorer pas select
    shiny::observeEvent(
      active$dexp,
      { active$label <- active$dexp }
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

      shiny::updateSelectInput(
        session,
        inputId = "latest_pas_select",
        choices = c("Select Sensor...", getPasLabels()),
        selected = active$label
      )


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
    #output$debug <- renderTable(active$pat)

    # L Col
    output$mini_table <- renderMiniTable()

    # - Overview Tab -
    output$leaflet <- renderLeaf()
    output$summary_plot <- renderBarPlot(plotType = "hourly_plot")
    output$cal_plot <- renderCalPlot()

    # - Comparison Tab -
    output$comp_leaflet <- renderLeaf()
    output$ws_ext <- renderExtFit()
    output$ws_comp <- renderMonitorComp()

    # - Raw tab -
    output$rose_plot <- renderRose()
    output$raw_plot <- renderMultiplot()

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
    output$help_text <- shiny::renderText(helpText())


  }

)

# CURRENT ISSUES:

# - URL querying

