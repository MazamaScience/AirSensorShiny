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

MazamaCoreUtils::logger.debug("------ ui() ------")

# ------ Define UI -------------------------------------------------------------

ui <- function(request) {

    shiny::navbarPage(

        # ------ Nav Bar -------------------------------------------------------
        title = tags$b("AirShiny (Beta)"),
        theme = shinythemes::shinytheme("yeti"),
        inverse = TRUE,
        id = "navtab",
        fluid = TRUE,
        collapsible = TRUE,
        position = "fixed-top",
        windowTitle = "AirShiny (Beta)",

        # ------ NavTab 1 - Explore --------------------------------------------
        shiny::tabPanel(
            # Pad the top of the page
            tags$style(type="text/css", "body {padding-top: 70px;}"),

            title = tags$b("Explore"),
            value = "explore",

            shiny::fluidRow(

                # ------ L Column ----------------------------------------------
                shiny::column(

                    width = 2,

                    shiny::wellPanel(

                        shinyjs::useShinyjs(),
                        shinyWidgets::setBackgroundColor(color = "#ecf0f5"),

                        shinybusy::add_busy_bar(
                            color = "#006687",
                            centered = TRUE
                        ),

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
                            label = tags$h4("Date Range"),
                            value = c(lubridate::now()-lubridate::days(7),
                                      lubridate::now()),
                            todayButton = FALSE,
                            addon = "none",
                            inline = TRUE,
                            separator = " to ",
                            range = FALSE,
                            maxDate = lubridate::now(tzone = TIMEZONE),
                            minDate = lubridate::ymd(20180102)
                        ),

                        shinyWidgets::radioGroupButtons(
                            inputId = "lookback_select",
                            label = tags$h4("View Past"),
                            choices = c( "3 Days" = 3,
                                         "7 Days" = 7,
                                         "15 Days" = 15,
                                         "30 Days" = 30 ),
                            justified = T,
                            direction = "vertical",
                            individual = F,
                            checkIcon = list(
                                yes = tags$i(class = "fa fa-check",
                                             style = "color: #5cc4ef"))

                        ),

                        tags$hr(),
                        tags$h5("Bookmark"),
                        shiny::bookmarkButton(
                            label = tags$small("Share..."),
                            icon = shiny::icon("share-square"),
                            title = "Copy Link to Share",
                            id = "exp_bookmark"
                        ),

                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$em(tags$small("Version ", VERSION))
                    ),

                    shiny::tableOutput("debug")
                ),

                # ------ R Column ----------------------------------------------
                shiny::column(
                    width = 8,

                    # ----- Tabs -----
                    shiny::tabsetPanel(
                        type = "tabs",
                        id = "tab_select",

                        # ---- Overview Tab ----
                        shiny::tabPanel(

                            title = tags$b("Overview"),
                            icon = shiny::icon("map-marked-alt"),
                            value = "main",

                            tags$br(),

                            shiny::column(
                                width = 12,
                                # Plot outputs
                                shiny::wellPanel(
                                    leaflet::leafletOutput(
                                        outputId = "leaflet",
                                        height = 420
                                    )
                                ),

                                shiny::wellPanel(
                                    dygraphs::dygraphOutput(
                                        "dySummary_plot",
                                        height = 330
                                    )
                                )
                            )
                        ),

                        # ---- Raw data tab ----
                        shiny::tabPanel(
                            title = tags$b("Raw Data"),
                            icon = shiny::icon("database"),
                            value = "raw",

                            tags$br(),
                            shiny::fluidRow(
                                shiny::column(
                                    width = 12,
                                    height = "800",
                                    tags$h4("Raw Data"),
                                    shiny::wellPanel(
                                        shiny::plotOutput(
                                            outputId = "raw_plot",
                                            height = "800"
                                        )
                                    )
                                )
                            ),

                            shiny::fluidRow(
                                shiny::column(
                                    width = 6,
                                    shiny::wellPanel(
                                        shiny::plotOutput(
                                            outputId = "ab_comp_plot"
                                        )

                                    )
                                ),

                                shiny::column(
                                    width = 6,
                                    shiny::wellPanel(
                                        shiny::plotOutput(
                                            outputId = "lm_comp_plot"
                                        )
                                    )
                                )
                            )
                        ),

                        # ----- Daily patterns tab -----
                        shiny::tabPanel(
                            title = tags$b("Daily Patterns"),
                            icon = shiny::icon("chart-bar"),
                            value = "dp",

                            tags$br(),

                            shiny::column(
                                width = 12,
                                tags$h4("Average Daily Patterns"),

                                shiny::textOutput(
                                    outputId = "pattern_title"
                                ),

                                shiny::wellPanel(
                                    shiny::plotOutput(
                                        outputId = "pattern_plot"
                                    )
                                ),

                                shiny::fluidRow(

                                    shiny::column(
                                        width = 5,

                                        tags$h4("Additional NOAA Weather Data"),
                                        shiny::wellPanel(
                                            DT::dataTableOutput(
                                                outputId = "met_table"
                                            )
                                        )
                                    ),

                                    shiny::column(
                                        width = 7,
                                        tags$h4("Wind Rose Plot"),
                                        shiny::wellPanel(
                                            shiny::plotOutput(
                                                outputId = "rose_plot"
                                            )
                                        )
                                    )
                                )
                            )
                        ),

                        # ----- Compare tab -----
                        shiny::tabPanel(
                            title = tags$b("Compare"),
                            icon = shiny::icon("balance-scale"),
                            value = "comp",
                            tags$br(),

                            shiny::fluidRow(
                                shiny::column(
                                    width = 12,
                                    shiny::wellPanel(
                                        leaflet::leafletOutput(
                                            outputId = "shiny_leaflet_comparison"
                                        )
                                    )
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    width = 5,
                                    shiny::wellPanel(
                                        DT::dataTableOutput(
                                            outputId = "comparison_table"
                                        )
                                    ),

                                    shiny::wellPanel(
                                        shiny::plotOutput(
                                            outputId = "ws_ext"
                                        )
                                    )
                                ),

                                shiny::column(
                                    width = 7,
                                    shiny::wellPanel(
                                        shiny::plotOutput(
                                            outputId = "ws_comp"
                                        )
                                    )
                                )
                            )
                        ),

                        # ---- Video tab ----
                        shiny::tabPanel(
                            title = tags$b("Community Timelapse"),
                            icon = shiny::icon("file-video"),
                            value = "anim",

                            tags$br(),
                            shiny::column(
                                width = 12,
                                shiny::wellPanel(
                                    shiny::uiOutput(
                                        outputId = "video_out"
                                    )
                                )
                            )
                        )
                    )
                ),

                # ----- Help Column ------
                shiny::column(
                    width = 2,
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

                    shiny::uiOutput(
                        outputId = "help_text"
                    )
                )
            )
        ),

        # ------ NavTab 2 - Data View ------------------------------------------

        shiny::tabPanel(

            title = tags$b("Data Viewer"),
            value = "dataview",
            shiny::fluidRow(
                # ------ L Column ----------------------------------------------
                shiny::column(

                    width = 2,

                    shiny::wellPanel(

                        shinyWidgets::pickerInput(
                            inputId = "de_pas_select",
                            label = tags$h4("Sensor"),
                            choices = "",
                            options = list(
                                `live-search` = TRUE,
                                title = "Select sensor...",
                                size = 7)
                        ),

                        shinyWidgets::airDatepickerInput(
                            inputId = "de_date_select",
                            label = tags$h4("Date Range"),
                            value = c(lubridate::now()-lubridate::days(7),
                                      lubridate::now()),
                            todayButton = FALSE,
                            addon = "none",
                            inline = TRUE,
                            separator = " to ",
                            range = FALSE,
                            maxDate = lubridate::now(tzone = TIMEZONE),
                            minDate = lubridate::ymd(20180102)
                        ),

                        shinyWidgets::radioGroupButtons(
                            inputId = "de_lookback_select",
                            label = tags$h4("View Past"),
                            choices = c( "3 Days" = 3,
                                         "7 Days" = 7,
                                         "15 Days" = 15,
                                         "30 Days" = 30 ),
                            justified = T,
                            direction = "vertical",
                            individual = F,
                            checkIcon = list(
                                yes = tags$i(class = "fa fa-check",
                                             style = "color: #5cc4ef"))

                        ),

                        tags$hr(),
                        shiny::downloadButton(
                            outputId = "download_data",
                            label = "Download"
                        ),
                        shiny::bookmarkButton(
                            label = tags$small("Share..."),
                            icon = shiny::icon("share-square"),
                            title = "Copy Link to Share",
                            id = "de_bookmark"
                        )
                    ),

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

                    DT::dataTableOutput(
                        outputId = "data_explorer"
                    )
                )
            )
        ),

        # ------ NavTab 3 - Latest ---------------------------------------------

        shiny::tabPanel(

            title = tags$b("Latest Data"),
            value = "latest",
            shiny::fluidRow(
                shiny::column(
                    width = 2,

                    shiny::wellPanel(
                        shinyWidgets::pickerInput(
                            inputId = "latest_comm_select",
                            label = tags$h4("Community"),
                            choices = c("All..." = "all", PAS_COMM),
                            options = list(title = "Select community...")
                        ),

                        shinyWidgets::pickerInput(
                            inputId = "latest_pas_select",
                            label = tags$h4("Sensor"),
                            choices = "",
                            options = list(
                                `live-search` = TRUE,
                                title = "Select Sensor...",
                                size = 7)
                        ),

                        # display selection mini table
                        shiny::tableOutput(outputId = "latest_mini_table"),

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
                    shiny::wellPanel(
                        shiny::plotOutput(
                            outputId = "aux_plot",
                            height = 850
                        )
                    )
                )
            )
        ),

        # ------ NavTab 4 - About ----------------------------------------------

        shiny::tabPanel(

            title = tags$b("About"),
            value = "about",
            shiny::column(width = 3),
            shiny::column(
                width = 6,
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

            ),
            shiny::column(width = 3)

        )

    )

}
