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
            panel_mod_ui("test")
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
                overview_mod_ui("test") )
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
                    calendar_mod_ui("test")
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
              raw_mod_ui("test")
            )
          )
        )
      )
    ),
    shiny::tabPanel(
      title = tags$b("View Data"),
      value = "dv"
    ),
    # Use ShinyJS
    shinyjs::useShinyjs(debug = TRUE),

    tags$style(type="text/css", "body {padding-top: 70px;}")
  )

}
