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

        # Capture date inputs
        get_dates <-
            function() {

                sd <- lubridate::ymd(input$date_selection) -
                    lubridate::ddays(as.numeric(input$lookback_days))

                ed <- lubridate::ymd(input$date_selection)

                return(c(sd, ed))

            }

        # ----- Functions -----

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
                    parseQueryString(session$clientData$url_search)

                # -- Define updates based on query
                # Update community selection based on query
                shiny::updateSelectInput(
                    session,
                    inputId = "comm_select",
                    selected = query[["communityId"]] )

            }

        # Create a (n)ew query string
        nquery <-
            function() {

                # -- Substitute spaces if true
                comm <-
                    ifelse(
                        grepl("\\s", input$comm_select ),
                        gsub("\\s","\\+", input$comm_select),
                        input$comm_select
                    )

                # -- Define queries to update based on input
                # Update the community string
                shiny::updateQueryString(
                    paste0(
                        "?communityId=",
                        comm)
                )


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

        # Render Multiplot
        renderMultiplot <-
            function() {

                shiny::renderPlot({

                    # NOTE: The current method is not filtering ANY outliers for
                    # NOTE: ANY of the plots - may be prone to change.
                    pat <- try(get_pat())
                    dates <- get_dates()

                    # Validate a pas selection has been made.
                    validate(
                        need(
                            input$leaflet_marker_click != "",
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

                    # Validate that pat has enough rows for aggregation.
                    validate(
                        need(
                            nrow(pat$data) > 60,
                            "An Error has occured. Please a different date."
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
                        paletteName = "Purple"
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

                    pat <- try(get_pat(de_selector = TRUE))

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
            function() {

                # Update selected pas based on leaflet selection
                pas <- get_pas()

                pas_valid_choices <-
                    pas[which(!stringr::str_detect(pas$label, "[Indoor]")),]

                shiny::updateSelectInput(
                    session,
                    inputId = "pas_select",
                    selected = input$leaflet_marker_click[1],
                    choices = pas_valid_choices$label
                )

                # Update URL communityId
                nquery()

            }

        # Update leaflet function for selected pas, or map click
        updateLeaf <-
            function() {

                # Get data
                lab <- input$pas_select
                ind <- which(PAS$label == lab)
                lng = PAS$longitude[ind]
                lat =  PAS$latitude[ind]

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

        # Update PAS select based on leaflet
        shiny::observeEvent(
            input$leaflet_marker_click,
            updateSelect()
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

        })

        # ----- Outputs -----

        # Leaflet Map
        output$leaflet <- renderLeaf()

        # Summary plot (below map)
        output$summary_plot <- renderBarPlot(plotType = "hourly_plot")

        # Data Table
        output$data_explorer <- renderDataExplorer()

        # Meta Table
        output$meta_explorer <- renderMetaExplorer()

        # Download button
        output$download_data <- downloadButton()

        # Leaflet selection label
        output$selected_label <- renderSelectedLabel()

    }

)
