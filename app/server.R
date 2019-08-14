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
        enddate = NULL,
        community = NULL,
        lookback = NULL,
        marker = NULL,
        compmarker = NULL,
        tab = NULL,
        navtab = NULL
      )

    # Update the active variable with an input variable
    updateActive <-
      function(update, with) active[[update]] <- input[[with]][1]

    # Update active pas
    shiny::observeEvent(
      label = "pas update",
      input$pas_select,
      updateActive("pas", "pas_select")
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

          # Create start of year date
          sd <- paste0(strftime(active$enddate, "%Y"), "0101")

          label <- active$pas

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
            AirSensor::pat_calendarPlot(pat)

          })

        })

      }

    # Render Selected mini table
    renderMiniTable <-
      function() {

        shiny::renderTable({

          # Get data
          lab <- active$pas
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

            pat <- active$pat#try(loadPat())#get_pat(selector = TRUE))
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
              active$pas != "Select Sensor...",
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

            sensor <- AirSensor::sensor_load(
              startdate = dates[1],
              enddate = dates[2]
            )

            sensor <- AirSensor::sensor_filterMeta(
              sensor,
              monitorID == active$pas
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

    # ----- Helper functions ---------------------------------------------------

    # Load the pat into the active pat (reactive expr only)
    loadPat <-
      function() {

        dates <- getDates()
        active$pat <-
          try(
            AirSensor::pat_load(
              active$pas,
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

          shiny::validate(need(active$pas != "Select Sensor...", "Please"))

          dates <- getDates()

          # Use the defined pas_valid_choices to apply filters if needed
          # i.e: remove any PAS that contains "Indoor" in its label

          closeId <-
            PAS$pwfsl_closestMonitorID[
              which(PAS$label==active$pas)
            ]

          close_ws <-
            PWFSLSmoke::monitor_load(
              dates[1],
              dates[2],
              monitorIDs = closeId
            )

          # Get data
          lab_pas <- active$pas
          ind <- which(PAS$label == lab_pas)
          lng_pas <- PAS$longitude[ind]
          lat_pas <-  PAS$latitude[ind]

          lab_ws <- close_ws$meta$monitorID
          lat_ws <- close_ws$meta$latitude
          lng_ws <- close_ws$meta$longitude

          lat <- mean(lat_pas, lat_ws)
          lng <- mean(lng_pas, lng_ws)

          # Interact with proxy comparison leaflet to avoid redraw
          leaflet::leafletProxy("comp_leaflet") %>%
            leaflet::clearGroup("ws_markers") %>%
            leaflet::clearGroup("pas_markers") %>%
            leaflet::flyTo(
              lng,
              lat,
              zoom = input$comp_leaflet_zoom
            ) %>%
            # Selected pas markers
            leaflet::addCircleMarkers(
              lng = lng_pas,
              lat = lat_pas,
              radius = 11,
              fillOpacity = 0,
              group = "pas_markers",
              options = leaflet::pathOptions(interactive = F),
              label = lab_pas,
              labelOptions = leaflet::labelOptions(
                noHide = TRUE,
                direction = "top"
              )
            ) %>%
            # Nearest monitor markers
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
        cPas <- active$pas

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


    # Trigger active pat update based on pas selection
    shiny::observeEvent(active$pas, loadPat())

    # Global observations
    shiny::observe({

      # BUGGY
      if ( active$tab == "main" || active$tab == "comp" ) {
        selected <- active$marker
      } else {
        selected <- active$marker <- active$pas
      }

      # Watch the pas_select based on the current active marker and community
      shiny::updateSelectInput(
        session,
        inputId = "pas_select",
        choices = c("Select Sensor...", getPasLabels()),
        selected = selected

      )
      # Watch the active variables to update the URL
      nquery()

    })

    # Trigger leaflet update based on marker and pas selections
    shiny::observeEvent(c(active$marker,active$tab), updateLeaf())
    shiny::observeEvent(c(active$pas, active$tab), updateLeaf(label = active$pas))

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
  }

)
