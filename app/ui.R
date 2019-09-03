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
#shiny::shinyUI(
ui <- function(request) {

    shiny::navbarPage(

        # ----- Nav Bar --------------------------------------------------------
        title = tags$b("AirShiny (Beta)"),
        theme = shinythemes::shinytheme("yeti"),
        inverse = TRUE,
        id = "navtab",
        fluid = TRUE,
        collapsible = TRUE,
        position = "fixed-top",
        windowTitle = "AirShiny (Beta)",

        # ----- NavTab 1 -------------------------------------------------------
        shiny::tabPanel(

            title = tags$b("Explorer"),
            value = "explore",

            shiny::fluidRow(

                # ----- L Column -----------------------------------------------
                shiny::column(

                    width = 2,

                    shiny::wellPanel(

                        shinyjs::useShinyjs(),

                        shinyWidgets::pickerInput(
                            inputId = "comm_select",
                            label = tags$h4("Community"),
                            choices = c("All..." = "all", PAS_COMM),
                            options = list(title = "Select community...")
                        ),

                        shinyWidgets::pickerInput(
                            inputId = "pas_select",
                            label = tags$h4("Sensor"),
                            choices = "",
                            options = list(
                                `live-search` = TRUE,
                                title = "Select sensor...",
                                size = 7)
                        ),

                        shinyWidgets::airDatepickerInput(
                            inputId = "date_select",
                            label = tags$h4("Date"),
                            value = lubridate::now(), todayButton = TRUE
                        ),

                        shinyWidgets::radioGroupButtons(
                            inputId = "lookback_select",
                            label = tags$h4("View Past"),
                            choices = c( "3 Days" = 3,
                                         "7 Days" = 7,
                                         "30 Days" = 30 ),
                            justified = TRUE
                        )
                    ),

                    # Display selection mini table
                    # shiny::tableOutput(outputId = "mini_table"),

                    shiny::tableOutput("debug"),

                    # Help Button
                    shinyWidgets::prettyToggle(
                        status_on = "success",
                        status_off = "primary",
                        inputId = "help_select",
                        label_on = "",
                        label_off = "",
                        outline = TRUE,
                        plain = TRUE,
                        icon_on = icon("question-circle", class = "regular"),
                        icon_off = icon("question-circle", class = "solid")
                    ),



                    shiny::htmlOutput(
                        outputId = "help_text"
                    ),
                    shiny::bookmarkButton()


                ),

                # ----- R Column -----------------------------------------------
                shiny::column(
                    width = 10,

                    # ---- Tabs ----
                    shiny::tabsetPanel(
                        type = "tabs",
                        id = "tab_select",

                        # --- Overview Tab ---
                        shiny::tabPanel(

                            title = tags$b("Overview"), icon = shiny::icon("home"),
                            value = "main",

                            tags$br(),

                            shiny::column(
                                width = 10,
                                # Plot outputs
                                leaflet::leafletOutput(
                                    outputId = "leaflet", height = 400
                                ),
                                # Summary Plot
                                # shiny::plotOutput(
                                #     outputId = "summary_plot", height = 300
                                # ),
                                dygraphs::dygraphOutput("dySummary_plot", height = 300)
                            ),
                            # Bar plot
                            shiny::column(
                                width = 2,
                                style = "margin-top: 0px;",
                                shiny::plotOutput(
                                    outputId = "cal_plot", height = 700,
                                    width = "100%"
                                )
                            )

                        ),

                        # --- Raw data tab ---
                        shiny::tabPanel(
                          title = tags$b("Raw Data"), icon = shiny::icon("database"),
                          value = "raw",

                          tags$br(),

                          shiny::column(
                            width = 8,
                            height = "800",
                            tags$h4("Raw Data"),
                            shiny::plotOutput(
                              outputId = "raw_plot",
                              height = "700"
                            )
                          ),

                          shiny::column(
                              width = 4,
                              shiny::fluidRow(
                                  tags$h4("Wind Rose Plot"),
                                  shiny::plotOutput(
                                      outputId = "rose_plot"
                                  ),
                                  tags$h4("Average Weather Data"),

                                  shiny::tableOutput(
                                      outputId = "met_table"
                                  )
                              )
                          )

                        ),

                        # --- Daily patterns tab ---
                        shiny::tabPanel(
                          title = tags$b("Daily Patterns"), icon = shiny::icon("chart-bar"),
                          value = "dp",

                          tags$br(),
                          tags$h4("Average Daily Patterns"),

                          shiny::plotOutput(
                            outputId = "pattern_plot"
                          )

                        ),

                        # --- Compare tab ---
                        shiny::tabPanel(
                          title = tags$b("Compare"), icon = shiny::icon("project-diagram"),
                          value = "comp",

                          tags$br(),

                          shiny::fluidRow(

                            shiny::column(
                              width = 9,
                              # Comparison Leaflet
                              leaflet::leafletOutput(
                                outputId = "shiny_leaflet_comparison",
                                height = 500
                              )
                            ),

                            shiny::column(
                              width = 3,
                              # Comparison table
                              shiny::tableOutput(
                                outputId = "comparison_table"
                              ),
                              # External fit plot
                              shiny::plotOutput(
                                outputId = "ws_ext"
                              )
                            )
                          ),

                          shiny::column(
                            width = 12,
                            # Monitor comparison plot
                            shiny::plotOutput(
                              outputId = "ws_comp"
                            )
                          )

                        ),

                        # --- Video tab ---
                        shiny::tabPanel(
                            title = tags$b("Community Timelapse"),icon = shiny::icon("file-video"),
                            value = "anim",

                            tags$br(),

                            shiny::uiOutput(
                                outputId = "video_out"
                            )
                        )

                    )

                )

            )

        ),

        # ----- NavTab 2 -------------------------------------------------------

        shiny::tabPanel(

            title = tags$b("Data Viewer"),
            value = "dataview",
            shiny::fluidRow(
                # ----- L Column -----------------------------------------------
                shiny::column(

                    width = 2,

                    shiny::wellPanel(

                        # shinyWidgets::pickerInput(
                        #     inputId = "de_comm_select",
                        #     label = tags$h4("Community"),
                        #     choices = c("All..." = "all",PAS_COMM),
                        #     options = list(title = "Select community...")
                        # ),

                        shinyWidgets::pickerInput(
                            inputId = "de_pas_select",
                            label = tags$h4("Sensor"),
                            choices = "",
                            options = list(
                                `live-search` = TRUE,
                                title = "Select sensor...",
                                size = 7)
                        ),
                        #shiny::wellPanel(
                        shinyWidgets::airDatepickerInput(
                            inputId = "de_date_select",
                            label = tags$h4("Date"),
                            value = lubridate::now()
                        ),

                        shinyWidgets::radioGroupButtons(
                            inputId = "de_lookback_select",
                            label = tags$h4("View Past"),
                            choices = c("3 Days" = 3, "7 Days" = 7, "30 Days" = 30),
                            justified = TRUE
                        ),

                        tags$br(),
                        shiny::downloadButton(
                            outputId = "download_data"
                        )
                    ),

                    # Display selection mini table
                    # shiny::tableOutput(outputId = "mini_table"),

                    shiny::tableOutput("de_debug"),

                    # Help button
                    shinyWidgets::prettyToggle(
                        status_on = "primary",
                        status_off = "primary",
                        inputId = "de_help_select",
                        label_on = "",
                        label_off = "",
                        outline = TRUE,
                        plain = TRUE,
                        icon_on = icon("question-circle", class = "regular"),
                        icon_off = icon("question-circle", class = "solid")
                    ),



                    shiny::textOutput(
                        outputId = "de_help_text"
                    )


                ),

                # Meta explorer
                shiny::column(
                    width = 9,
                    shiny::tableOutput(
                        outputId = "meta_explorer"
                    )
                ),

                # Data explorer
                shiny::column(
                    width = 10,
                    shiny::dataTableOutput(
                        outputId = "data_explorer"
                    )
                )
            )
        ),

        # ----- NavTab 3 -------------------------------------------------------

        shiny::tabPanel(

            title = tags$b("Latest Data"),
            value = "latest",
            shiny::fluidRow(
                shiny::column(

                    width = 2,

                    # Latest leaflet display
                    # leaflet::leafletOutput(
                    #     outputId = "latest_leaflet", height = 400
                    # ),

                    shiny::wellPanel(
                        shinyWidgets::pickerInput(
                            inputId = "latest_comm_select",
                            label = tags$h4("Community"),
                            choices = c("All..." = "all",PAS_COMM),
                            options = list(title = "Select community...")
                        ),

                        shinyWidgets::pickerInput(
                            inputId = "latest_pas_select",
                            label = tags$h4("Sensor"),
                            choices = "",
                            options = list(
                                `live-search` = TRUE,
                                title = "Select sensor...",
                                size = 7)
                        ),

                        # Display selection mini table
                        shiny::tableOutput(outputId = "Latest_mini_table"),

                        tags$br(),

                        shiny::actionButton(
                            inputId = "loadButton",
                            label = "Load Latest"
                        )
                    ),

                    # Help button
                    shinyWidgets::prettyToggle(
                        status_on = "primary",
                        status_off = "primary",
                        inputId = "latest_help_select",
                        label_on = "",
                        label_off = "",
                        outline = TRUE,
                        plain = TRUE,
                        icon_on = icon("question-circle", class = "regular"),
                        icon_off = icon("question-circle", class = "solid")
                    )

                ),


                shiny::column(
                    width = 10,


                    # dygraphs::dygraphOutput(
                    #     outputId = "dygraph_plot"
                    # ),
                    #

                    shiny::plotOutput(
                        outputId = "aux_plot",
                        height = 850
                    )
                )

            )
        ),

        # ----- NavTab 4 -------------------------------------------------------

        shiny::tabPanel(

            title = tags$b("About"),
            value = "about",
            shiny::column(width = 3),
            shiny::column(
              width = 5,
              fluidRow(
                tags$h2("About AirShiny"), align = "center"),
                tags$h3("Purpose"),
                tags$p(
                      "The AirShiny App as well as the AirSensor R-Package were developed through a
                      collaboration between the South Coast Air Quality Management District (South Coast AQMD), a regional U.S.
                      Governmental Agency in California, USA and Mazama Science, a software company in Seattle, WA. This tool is
                      intended to support data exploration and analysis by community members participating in the US EPA funded
                      STAR Grant at the South Coast AQMD, entitled “Engage, Educate and Empower California Communities on the Use
                      and Applications of Low-cost Air Monitoring Sensors”. Funding for the development of this tool was provided
                      through this US EPA STAR Grant (RD83618401)."
                ),
              tags$h3("QA/QC Procedures"),
              tags$p(
                    "Description of QA/QC Procedures: All data provided throughout this application
                    (with the exception of the data displayed on the “Raw Data” tab on the Explorer page and the data shown on the “Latest Data” page) has
                    undergone the following QA/QC procedures: (1) removal of values outside of the specifications for the sensors, as
                    defined by the manufacturer, (2) pollutant values for Channel A and Channel B are averaged on an hourly basis,
                    (3) if the proportion of points contributing to the hourly average meets the minimum requirement and the hourly
                    averages are judged to be not statistically different, according to a student’s t-test, then the hourly averages for
                    Channel A and B are averaged together – producing a single value for each hour. More detail on the procedures
                    and functions used is available in the AirSensor R-Package documentation."
              ),
              tags$h3("Disclaimer"),
              tags$p(
                    "Disclaimer: This tool is intended to be used for educational and informational purposes only. Furthermore, the
                    code used to build this tool, the QA/QC procedures, and the different features of this tool may be subject to
                    revision at any time depending on the needs of the project."
              )

            )

        ),

        # ----- Misc -----------------------------------------------------------

        tags$style(type="text/css", "body {padding-top: 70px;}")
        #shinythemes::themeSelector()

    )

}#)
