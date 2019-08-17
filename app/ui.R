#
# This is the user-interface definition of AirShiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about Shiny apps here:
#
#    http://shiny.rstudio.com/
#
# - Mazama Science
#

MazamaCoreUtils::logger.debug("----- ui() -----")

# ----- Define UI --------------------------------------------------------------
shiny::shinyUI(
    shiny::navbarPage(

        # ----- Nav Bar --------------------------------------------------------
        title = "AirShiny (Beta)",
        theme = shinythemes::shinytheme("lumen"),
        inverse = TRUE,
        id = "navtab",

        # ----- NavTab 1 -------------------------------------------------------
        shiny::tabPanel(

            title = "Explorer",
            value = "explore",

            shiny::fluidRow(

                # ----- L Column -----------------------------------------------
                shiny::column(

                    width = 2,

                    # Community Selection input
                    shiny::selectInput(
                        inputId = "comm_select",
                        label = "Community",
                        choices = c("All..." = "all",
                                    PAS_COMM)
                    ),

                    # PAS selection input
                    shiny::selectInput(
                        inputId = "pas_select",
                        label = "Sensor",
                        choices = ""
                    ),

                    # End Date input
                    shiny::dateInput(
                        inputId = "date_select",
                        label = "Date"
                    ),

                    # Lookback interval
                    shiny::radioButtons(
                        inputId = "lookback_select",
                        label = "Look back",
                        choices = c("3 Days" = 3,
                                    "7 Days" = 7,
                                    "30 Days" = 30)
                    ),

                    # Display selection mini table
                    shiny::tableOutput(outputId = "mini_table"),

                    shiny::tableOutput("debug")

                ),

                # ----- R Column -----------------------------------------------
                shiny::column(
                    width = 10,

                    # ---- Tabs ----
                    shiny::tabsetPanel(
                        type = "tabs",
                        id = "tab_select",

                        # --- Leaflet Tab ---
                        shiny::tabPanel(

                            title = "Overview",
                            value = "main",

                            shiny::column(
                                width = 10,
                                # Plot outputs
                                leaflet::leafletOutput(
                                    outputId = "leaflet", height = 600
                                ),
                                # Summary Plot
                                shiny::plotOutput(
                                    outputId = "summary_plot", height = 200
                                )
                            ),
                            shiny::column(
                                width = 2, style = "margin-top: 0px;",
                                shiny::plotOutput(
                                    outputId = "cal_plot", height = 800
                                )
                            )

                        ),

                        # --- Video tab ---
                        shiny::tabPanel(
                            title = "Animation",
                            value = "anim",
                            shiny::uiOutput(
                                outputId = "video_out"
                            )
                        ),

                        # --- Compare tab ---
                        shiny::tabPanel(
                            title = "Compare",
                            value = "comp",
                            # Comparison Leaflet
                            leaflet::leafletOutput(
                                outputId = "comp_leaflet"
                            ),
                            shiny::column(
                                width = 8,
                                # Monitor comparison plot
                                shiny::plotOutput(
                                    outputId = "ws_comp"
                                )
                            ),
                            shiny::column(
                                width = 4,
                                # External fit plot
                                shiny::plotOutput(
                                    outputId = "ws_ext"
                                )
                            )

                        ),

                        shiny::tabPanel(
                            title = "Daily Patterns",
                            value = "dp",
                            shiny::plotOutput(
                                outputId = "pattern_plot"
                            )

                        ),

                        # --- Raw & Weather data tab ---
                        shiny::tabPanel(
                            title = "Raw Data",
                            value = "raw",
                            shiny::plotOutput(
                                outputId = "raw_plot"
                            ),
                            shiny::column(
                                width = 6,
                                shiny::plotOutput(
                                    outputId = "rose_plot"
                                )
                            )
                        )

                    )

                )

            )

        ),

        # ----- NavTab 2 -------------------------------------------------------

        shiny::tabPanel(

            title = "Data Viewer",
            value = "dataview",

            # PAS selection input
            shiny::column(
                width = 2,
                shiny::selectInput(
                    inputId = "de_pas_select",
                    label = "Sensor",
                    choices = ""
                )
            ),

            # Meta explorer
            shiny::column(
                width = 9,
                shiny::tableOutput(
                    outputId = "meta_explorer"
                )
            ),

            # Download Button
            shiny::column(
                width = 1,
                shiny::downloadButton(
                    outputId = "download_data"
                )
            ),

            # Data explorer
            shiny::dataTableOutput(
                outputId = "data_explorer"
            )

        ),

        # ----- NavTab 3 -------------------------------------------------------

        shiny::tabPanel(

            title = "Latest Data",
            value = "latest",

            shiny::column(

                width = 2,


                leaflet::leafletOutput(
                    outputId = "latest_leaflet", height = 400
                ),

                # Community Selection input
                shiny::selectInput(
                    inputId = "latest_comm_select",
                    label = "Community",
                    choices = c("All..." = "all",
                                PAS_COMM)
                ),

                # PAS selection input
                shiny::selectInput(
                    inputId = "latest_pas_select",
                    label = "Sensor",
                    choices = ""
                ),

                # Lookback interval
                # shiny::radioButtons(
                #     inputId = "latest_hours_select",
                #     label = "Look back",
                #     choices = c("Last Hour" = 1,
                #                 "Last" = 7,
                #                 "30 Days" = 30)
                # ),

                # Display selection mini table
                shiny::tableOutput(outputId = "Latest_mini_table"),
                shiny::actionButton(
                    inputId = "loadButton",
                    label = "Load Latest"
                )
            ),

            shiny::column(
                width = 10,


                # dygraphs::dygraphOutput(
                #     outputId = "dygraph_plot"
                # ),
                #

                shiny::plotOutput(
                    outputId = "aux_plot", height = 850
                )
            )

        ),

        # ----- NavTab 4 -------------------------------------------------------

        shiny::tabPanel(

            title = "About",
            value = "about"

            # TODO: Get an about section.

        )

    )

)
