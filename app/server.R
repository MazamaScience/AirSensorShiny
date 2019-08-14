# ----- Define Server Logic ----------------------------------------------------

shiny::shinyServer(
  function(input, output, session) {

    # ----- Reactive Controls -----
    # Define active user selections
    active <-
      shiny::reactiveValues(
        pas = NULL,
        pat = NULL,
        enddate = NULL,
        community = NULL,
        lookback = NULL,
        marker = NULL
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

    shiny::observeEvent(
      label = "leaflet marker update",
      input$leaflet_marker_click,
      updateActive("marker", "leaflet_marker_click")
    )



    # Get the dates
    getDates <-
      shiny::reactive({

        sd <- lubridate::ymd(active$enddate) -
          lubridate::ddays(as.numeric(active$lookback))

        ed <- lubridate::ymd(active$enddate)

        return(c(sd, ed))

      }, label = "get dates")

    # Load the pat from active selection
    loadPat <-
        shiny::reactive({
          AirSensor::pat_load(
          active$pas,
          getDates()[1],
          getDates()[2]
        )

      }, label = "load pat")

    # Get the valid pas selectables
    getPasLabels <-
      shiny::reactive({

        if ( active$community == "all" ) {
          return(PAS$label[which(!is.na(PAS$communityRegion))])
        } else {
          return(PAS$label[grepl(active$community, PAS$communityRegion)])
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

    # ----- Render Handling -----

    # ---------- Overview Tab ------------
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
          pat <- loadPat()
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
              pat_load(
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

    # Update leaflet function for selected pas, or map click
    updateLeaf <-
      function(label = NULL) {

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
            options = leaflet::pathOptions(interactive = F)
          ) %>%
          leaflet::addPopups(lng, lat, popup = content)

      }

    shiny::observe({
          shiny::updateSelectInput(
            session,
            inputId = "pas_select",
            choices = c("Select Sensor...", getPasLabels()),
            selected = active$marker

          )

      #updateLeaf()

    })
    shiny::observeEvent(active$marker, updateLeaf())
    shiny::observeEvent(active$pas, updateLeaf(active$pas))


    # ----- Outputs -----
    #$output$debug <- renderTable(loadPat()$data)

    # - Overview Tab -
    output$leaflet <- renderLeaf()
    output$summary_plot <- renderBarPlot(plotType = "hourly_plot")
    output$cal_plot <- renderCalPlot()
  }

)
