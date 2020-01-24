#' AirSensor DataViewer User Interface
#'
#' @param request
ui <- function(request) {

  shiny::navbarPage(
    # ------ Nav Bar -------------------------------------------------------
    title = tags$b("AirSensor DataViewer (Beta)"),
    theme = shinythemes::shinytheme("yeti"),
    inverse = TRUE,
    id = "navbar",
    fluid = TRUE,
    collapsible = TRUE,
    position = "fixed-top",
    windowTitle = "AirSensor Viewer (Beta)",
    # ------ NavTab 1 - Explore --------------------------------------------
    shiny::tabPanel(
      title = tags$b("Explore"),
      value = "explore",
      shiny::fluidRow(
        # ------ L Column ----------------------------------------------
        shiny::column(
          width = 2,
          shiny::wellPanel(
            panel_mod_ui("explore")
          )
        ),
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
              value = "overview",
              tags$br(),
              shiny::column(
                width= 12,
                overview_mod_ui("explore") )
            ),
            # ---- Calendar tab ----
            shiny::tabPanel(
              title = tags$b("Calendar"),
              icon = shiny::icon("calendar-alt"),
              value = "calendar",
              tags$br(),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  shiny::wellPanel(
                    calendar_mod_ui("explore")
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
              raw_mod_ui("explore")
            ),
            # ----- Daily patterns tab -----
            shiny::tabPanel(
              title = tags$b("Daily Patterns"),
              icon = shiny::icon("chart-bar"),
              value = "dp",

              tags$br(),
              pattern_mod_ui("explore")
            ),
            # ----- Compare tab -----
            shiny::tabPanel(
              title = tags$b("Compare"),
              icon = shiny::icon("balance-scale"),
              value = "comp",
              tags$br(),
              comparison_mod_ui("explore")
            ),
            # ---- Video tab ----
            shiny::tabPanel(
              title = tags$b("Community Timelapse"),
              icon = shiny::icon("file-video"),
              value = "anim",
              tags$br(),
              video_mod_ui("explore")
            )
          )
        )
      )
    ),
    shiny::tabPanel(
      title = tags$b("View Data"),
      value = "dv",
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shiny::wellPanel(
            panel_mod_ui("dv")
          )
        ), dataview_mod_ui("dv")
      )
    ),
    # Use ShinyJS
    shinyjs::useShinyjs(debug = TRUE),

    tags$style(type="text/css", "body {padding-top: 70px;}")
  )

}
