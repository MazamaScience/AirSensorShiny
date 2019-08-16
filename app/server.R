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
        dexp = NULL
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
        active$pas <- PAS[grepl(active$label,PAS$label),][1,]
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

    # ----- Render Handling ----------------------------------------------------

    # Render Map
    renderLeaf <-
      function() {

        leaflet::renderLeaflet({

          pas <- getCommunitySensors()

          # Use the defined pas_valid_choices to apply filters if needed
          # i.e: remove any PAS that contains "Indoor" in its label

          pas_valid_choices <-
            pas[which(!stringr::str_detect(pas$label, "[Indoor]")),]

          utils_leaflet(
            pas = pas_valid_choices,
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

          handleError(active$label != "Select Sensor...", "")


          # Create start of year date
          sd <- paste0(strftime(active$enddate, "%Y"), "0101")

          label <- active$label

          showLoad({
            # Load annual pat to now
            pat <-
              AirSensor::pat_load(
                label,
                startdate = sd,
                enddate = Sys.Date()
              )

            shiny::incProgress(0.65)

            # Calendar plot
           try(AirSensor::pat_calendarPlot(pat))

          })

        })

      }

    # Render Selected mini table
    renderMiniTable <-
      function() {

        shiny::renderTable({

          # Get data
          lab <- active$label
          ind <- which(PAS$label == lab)
          lng = PAS$longitude[ind]
          lat =  PAS$latitude[ind]

          dplyr::tibble(
            "Sensor" = lab,
            "Latitude" = lat,
            "Longitude" = lng
          )

        })

      }

    # Render the monitor comparison plot
    renderMonitorComp <-
      function() {

        shiny::renderPlot({

          showLoad({

            pat <- active$pat
            dates <- getDates()

            shiny::incProgress(0.66)

            AirSensor::pat_monitorComparison(pat)

          })

        })

      }

    # Render monitor & pas external fit plot
    renderExtFit <-
      function() {

        shiny::renderPlot({

          pat <- active$pat#try(loadPat())
          dates <- getDates()

          utils_externalFit(pat)

        })

      }

    # Render Multiplot
    renderMultiplot <-
      function() {

        shiny::renderPlot({

          # NOTE: The current method is not filtering ANY outliers for
          # NOTE: ANY of the plots - may be prone to change.
          pat <- active$pat#try(loadPat())
          dates <- getDates()

          # Validate a pas selection has been made.
          validate(
            need(
              active$label != "Select Sensor...",
              "Select a Purple Air Sensor"
            )
          )

          # Validate that pat is returned.
          validate(
            need(
              pat,
              "An Error has occured. Please select another sensor."
            )
          )

          AirSensor::pat_multiplot(pat)

        })

      }

    # Render rose plot
    renderRose <-
      function() {

        shiny::renderPlot({
          showLoad({
            dates <- getDates()

            sensor <-
              AirSensor::sensor_filterMeta(
                AirSensor::sensor_load(startdate = dates[1],enddate = dates[2]),
                monitorID == active$label
              )

            # sensor <- AirSensor::sensor_filterMeta(
            #   sensor,
            #   monitorID == active$label
            # )

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

          pat <- active$pat

          community <- active$pas$communityRegion
            #PAS$communityRegion[which(PAS$label == pat$meta$label)][1]

          meta <-
            data.frame(
              "Community" = community,
              "Sensor Type" = pat$meta$sensorType,
              "Longitude" = pat$meta$longitude,
              "Latitude" = pat$meta$latitude,
              "State" = pat$meta$stateCode,
              "Country" = pat$meta$countryCode,
              "Timezone" = pat$meta$timezone
            )

          return(meta)

        })

      }

    # Render Data Table Explorer
    renderDataExplorer <-
      function() {

        shiny::renderDataTable({

          showLoad(shiny::incProgress(0.7))

          return(active$pat$data)

        })

      }

    # ----- Helper functions ---------------------------------------------------

    # Handle download button
    downloadButton <-
      function() {

        shiny::downloadHandler(
          filename = function() {

            dates <- getDates()
            pat <- active$pat#get_pat(de_selector = TRUE)

            paste0(
              pat$meta$label,
              "_",
              dates[1],
              "_",
              dates[2],
              ".csv"
            )

          },

          content = function(file) {

            pat <- active$pat#get_pat(selector = TRUE)
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

        # If on main tab
        if ( active$tab == "main" ) {

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


          # Interact with proxy leaflet to avoid redraw
          leaflet::leafletProxy("leaflet") %>%
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

        # If on the comparison tab
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

          nearestMonitor_Id <- #shiny::isolate(active$pas$pwfsl_closestMonitorID)
            PAS$pwfsl_closestMonitorID[grepl(active$label, PAS$label)][1]

          nearestMonitor <-
            try(
              AirSensor::pwfsl_load(
                monitorIDs = nearestMonitor_Id,
                startdate = dates[1],
                enddate = dates[2]
              )
            )

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

    # ----- Reactive Observations ----------------------------------------------
    # NOTE: For use with low-hierarchy update functions and features.

    # Trigger active pat update based on pas selection, lookback, enddate, dexp
    shiny::observeEvent(
      c(active$label, active$lookback, active$enddate, active$dexp),
      loadPat()
    )

    # Global observations
    shiny::observe({

      # BUGGY
      if ( active$tab == "main" || active$tab == "comp" ) {
        selected <- active$marker
      } else {
        selected <- active$marker <- active$label
      }

      # Watch the pas_select based on the current active marker and community
      shiny::updateSelectInput(
        session,
        inputId = "pas_select",
        choices = c("Select Sensor...", getPasLabels()),
        selected = selected

      )

      if ( active$navtab == "dataview" ) {

        shiny::updateSelectInput(
          session,
          inputId = "de_pas_select",
          selected = active$dexp,
          choices = c("Select Sensor...",
                      PAS$label[!is.na(PAS$communityRegion) &
                                  !grepl("(<?\\sB)$", PAS$label) &
                                  PAS$DEVICE_LOCATIONTYPE != "inside"])
        )
        # Update the pas selector on main??
        shiny::updateSelectInput(
          session,
          inputId = "pas_select",
          selected = active$dexp

        )

      }

      # Watch the active variables to update the URL
      nquery()

    })

    # Trigger leaflet update based on marker and pas selections
    shiny::observeEvent(c(active$marker,active$tab), updateLeaf())
    shiny::observeEvent(c(active$label, active$tab), updateLeaf(label = active$label))

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

    # - Data Explorer -
    output$data_explorer <- renderDataExplorer()
    output$meta_explorer <- renderMetaExplorer()
    output$download_data <- downloadButton()

  }

)

# CURRENT ISSUES:
# - Selecting a pas from selection box while in view of comparison or main leaflet is buggy
# - Updating the pas selection from data explorer. Keep consistent.
# - Errors...errors everywhere
# - No daily patterns plot

