#
# This is the server logic of AirShiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about Shiny applications here:
#
#    http://shiny.rstudio.com/
#
# - Mazama Science
#

MazamaCoreUtils::logger.debug("----- server() -----")

# ----- Define Server Logic ----------------------------------------------------

shiny::shinyServer(
    function(input, output, session) {

        output$activeTab <- reactive({

            return(input$tab_select)

        })

        shiny::outputOptions(output, "activeTab", suspendWhenHidden = FALSE)

        # ----- Functions -----

        # Capture date inputs
        get_dates <-
            function() {

                sd <- lubridate::ymd(input$date_selection) -
                    lubridate::ddays(as.numeric(input$lookback_days))

                ed <- lubridate::ymd(input$date_selection)

                return(c(sd, ed))

            }

        # Capture PAT selection
        get_pat <-
            function(selector = FALSE, de_selector = FALSE) {

                MazamaCoreUtils::logger.debug(" # get_pat #")

                dates <- get_dates()

                if ( selector ) {

                    pat <-
                        AirSensor::pat_load(
                            label = input$pas_select,
                            startdate = dates[1],
                            enddate = dates[2]
                        )

                } else if ( de_selector ) {
                    pat <-
                        AirSensor::pat_load(
                            label = input$de_pas_select,
                            startdate = dates[1],
                            enddate = dates[2]
                        )

                } else {

                    pat <-
                        AirSensor::pat_load(
                            label = input$leaflet_marker_click[1],
                            startdate = dates[1],
                            enddate = dates[2]
                        )

                }

                return(pat)

            }

        # Capture selected PAS based on community selection
        get_pas <-
            function() {

                if ( input$comm_select == "all" )  {

                    pas <- PAS[which(!is.na(PAS$communityRegion)),]

                } else {

                    pas <-
                        PAS[which(
                            stringr::str_detect(
                                PAS$communityRegion,
                                input$comm_select)
                        ),]

                }

                pas$label <-
                    stringr::str_split(
                        string = pas$label,
                        pattern = " ",
                        simplify = TRUE
                    )[,1]

                return(pas)

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
                    inputId = "navbar",
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
                        grepl("\\s", input$comm_select ),
                        gsub("\\s","\\+", input$comm_select),
                        input$comm_select
                    )
                cNav <- input$navbar
                # Current tab
                cTab <-
                    input$tab_select

                # Current pas select
                cPas <- input$pas_select

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

        # Show loading widget
        showLoad <-
            function(FUN) {

                shiny::withProgress(
                    expr = FUN,
                    value = 0.3,
                    message = "Loading...",
                    detail = "Please Wait."
                )

                return(FUN)

            }

        # Render Bar Plot
        renderBarPlot <-
            function(plotType) {

                shiny::renderPlot({

                    # NOTE: The current method is not filtering ANY outliers for
                    # NOTE: ANY of the plots - may be prone to change.

                    # get pat based on pas selection
                    pat <- try(get_pat(selector = TRUE))
                    dates <- get_dates()

                    # Validate that pat is returned.
                    validate(
                        need(
                            pat,
                            "An Error has occured. Please select another sensor."
                        )
                    )

                    # Validate that pat has enough rows for aggregation.
                    validate(
                        need(
                            nrow(pat$data) > 60,
                            "An Error has occured. Please a different date."
                        )
                    )

                    if ( plotType == "daily_plot" ) {

                        utils_barplot(
                            pat,
                            period = "1 day",
                            startdate = dates[1],
                            enddate = dates[2]
                        )

                    } else if ( plotType == "multi_plot" ) {

                        AirSensor::pat_multiplot(pat)

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

                    # Validate that pas is selected.
                    validate(
                        need(
                            input$pas_select,
                            ""
                        )
                    )

                    # Create start of year date
                    sd <- paste0(strftime(input$date_selection, "%Y"), "0101")

                    label <- input$pas_select

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

        # Render Multiplot
        renderMultiplot <-
            function() {

                shiny::renderPlot({

                    # NOTE: The current method is not filtering ANY outliers for
                    # NOTE: ANY of the plots - may be prone to change.
                    pat <- try(get_pat(selector = TRUE))
                    dates <- get_dates()

                    # Validate a pas selection has been made.
                    validate(
                        need(
                            input$pas_select != "",
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

        # Render Map
        renderLeaf <-
            function() {

                leaflet::renderLeaflet({

                    pas <- get_pas()

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

        # Render Selected label meta table
        renderSelectedLabel <-
            function() {

                shiny::renderTable({

                    validate(need(input$pas_select != "", ""))

                    # Get data
                    lab <- input$pas_select
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

                        pat <- try(get_pat(selector = TRUE))
                        dates <- get_dates()

                        shiny::incProgress(0.66)

                        AirSensor::pat_monitorComparison(pat)

                    })

                })

            }

        # Render monitor & pas external fit plot
        renderExtFit <-
            function() {

                shiny::renderPlot({

                    pat <- try(get_pat(selector = TRUE))
                    dates <- get_dates()

                    utils_externalFit(pat)

                })

            }

        # Render video
        renderVideo <-
            function() {

                shiny::renderUI({

                    baseUrl <-
                        "http://smoke.mazamascience.com/data/PurpleAir/videos/"
                    year <-
                        strftime(input$date_selection, "%Y")
                    mm <-
                        strftime(input$date_selection, "%m")
                    dd <-
                        strftime(input$date_selection, "%d")
                    hh <- "09"
                    comm <- input$comm_select

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
        # Render rose plot
        renderRose <-
            function() {

                shiny::renderPlot({
                    showLoad({
                        dates <- get_dates()

                        sensor <- sensor_load(
                            startdate = dates[1],
                            enddate = dates[2]
                        )

                        sensor <- sensor_filterMeta(
                            sensor,
                            monitorID == input$pas_select
                        )

                        shiny::incProgress(0.44)

                        AirSensor::sensor_pollutionRose(sensor)
                    })

                })

            }

        # Render Meta table explorer
        renderMetaExplorer <-
            function() {

                shiny::renderTable({

                    validate(need(input$de_pas_select != "", ""))

                    pat <- try(get_pat(de_selector = TRUE))

                    pas <- get_pas()

                    # Validate that pat is returned if not display
                    validate(
                        need(
                            pat,
                            ""
                        )
                    )

                    community <-
                        pas$communityRegion[which(pas$label == pat$meta$label)][1]

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

                    showLoad({

                        pat <- try(get_pat(de_selector = TRUE))

                        shiny::incProgress(0.6)

                        # Validate a pas selection has been made
                        validate(
                            need(
                                input$de_pas_select != "",
                                "Select a Sensor"
                            )
                        )

                        # Validate that pat is returned
                        validate(
                            need(
                                pat,
                                "An Error has occured. Please select another sensor."
                            )
                        )

                        data <- pat$data
                    })

                    return(data)

                })

            }

        # Handle download button
        downloadButton <-
            function() {

                shiny::downloadHandler(
                    filename = function() {

                        dates <- get_dates()
                        pat <- get_pat(de_selector = TRUE)

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

                        pat <- get_pat(selector = TRUE)
                        write.csv(pat$data, file = file)

                    }

                )

            }

        # Update pas selections from map click
        updateSelect <-
            function(look = "leaflet") {

                # Update selected pas based on leaflet selection
                pas <- get_pas()

                pas_valid_choices <-
                    pas[which(!stringr::str_detect(pas$label, "[Indoor]")),]

                if ( look == "leaflet" ) {

                    shiny::updateSelectInput(
                        session,
                        inputId = "pas_select",
                        selected = input$leaflet_marker_click[1],
                        choices = pas_valid_choices$label
                    )

                } else if ( look == "comp" ) {

                    shiny::updateSelectInput(
                        session,
                        inputId = "pas_select",
                        selected = input$comp_leaflet_marker_click[1],
                        choices = pas_valid_choices$label
                    )

                }

                # Update URL communityId
                nquery()

            }

        # Update leaflet function for selected pas, or map click
        updateLeaf <-
            function() {

                # Get data
                lab <- input$pas_select
                ind <- which(PAS$label == lab)
                lng <- PAS$longitude[ind]
                lat <-  PAS$latitude[ind]

                # Create popup HTML
                html <-
                    list(
                        "lab" = paste("<b><a>",lab,"</a></b>"),
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
                        html$lab,
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

        # Update the comparison leaflet
        updateCompLeaf <-
            function() {

                validate(need(input$pas_select != "", ""))
                #pas <- get_pas()
                dates <- get_dates()

                # Use the defined pas_valid_choices to apply filters if needed
                # i.e: remove any PAS that contains "Indoor" in its label

                closeId <-
                    PAS$pwfsl_closestMonitorID[
                        which(PAS$label==input$pas_select)
                    ]

                close_ws <-
                    PWFSLSmoke::monitor_load(
                        dates[1],
                        dates[2],
                        monitorIDs = closeId
                    )

                # Get data
                lab_pas <- input$pas_select
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

        # Update bar plot from plot type selection
        updateBarPlot <-
            function() {

                output$selected_plot <-
                    renderBarPlot(plotType = input$plot_type_select)

            }

        # ----- Observations to update -----

        # Update based on URL
        shiny::observeEvent(
            session$clientData$url_search,
            fquery()
        )

        # Update URL and selectable PAS based on community selection
        shiny::observeEvent(
            input$comm_select,
            updateSelect()
        )

        # Update bar plot output based on plot type selection
        shiny::observeEvent(
            input$plot_type_select,
            updateBarPlot()
        )

        # Update leaflet based on PAS select
        shiny::observeEvent(
            input$pas_select,
            updateLeaf()
        )

        # Update leaflet based on if tab select on main tab
        shiny::observeEvent(
            input$tab_select == "main",
            updateLeaf()
        )

        # Update comparison leaflet based on PAS select
        # shiny::observeEvent(
        #     input$pas_select,
        #     updateCompLeaf()
        # )

        # Update comparison leaflet if tab select on compare tab
        shiny::observeEvent(
            input$tab_select == "comp",
            updateCompLeaf()
        )

        # Update PAS select based on leaflet
        shiny::observeEvent(
            input$leaflet_marker_click,
            updateSelect()
        )

        # Update PAS select based on comparison leaflet \
        shiny::observeEvent(
            input$comp_leaflet_marker_click,
            updateSelect("comp")
        )

        # Global observations
        shiny::observe({

            # Update selected data explorer pas based on leaflet selection
            pas <- get_pas()

            pas_valid_choices <-
                pas[which(!stringr::str_detect(pas$label, "[Indoor]")),]

            shiny::updateSelectInput(
                session,
                inputId = "de_pas_select",
                selected = input$pas_select,
                choices = pas_valid_choices$label
            )

            # Follow and update the url
            nquery()

        })

        # ----- Outputs -----

        # Leaflet Map
        output$leaflet <- renderLeaf()

        # Summary plot (below map)
        output$summary_plot <- renderBarPlot(plotType = "hourly_plot")

        # Calendar plot
        output$cal_plot <- renderCalPlot()

        # Comparison map
        output$comp_leaflet <- renderLeaf()

        # Monitor comparison
        output$ws_comp <- renderMonitorComp()

        # Monitor external fit
        output$ws_ext <- renderExtFit()

        # Data Table
        output$data_explorer <- renderDataExplorer()

        # Meta Table
        output$meta_explorer <- renderMetaExplorer()

        # Download button
        output$download_data <- downloadButton()

        # Leaflet selection label
        output$selected_label <- renderSelectedLabel()

        # Video
        output$video_out <- renderVideo()

        # Raw output
        output$raw_plot <- renderMultiplot()

        # Rose plot output
        output$rose_plot <- renderRose()

        #active_pas <- shiny::reactive(input$pas_select)
        active_pat <- shiny::reactive({
            dates <- get_dates()
            pat <- pat_load(label  = input$pas_select,
                            startdate = dates[1],
                            enddate = dates[2])
            return(data.frame(pat$data$datetime))

            })

        output$debug <- shiny::renderTable(active_pat)

    }

)
