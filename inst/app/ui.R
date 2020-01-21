#' AirSensor DataViewer User Interface
#'
#' @param request
ui <- function(request) {

  shiny::navbarPage(
    # ------ Nav Bar -------------------------------------------------------
    title = tags$b("AirSensor DataViewer (Beta)"),
    theme = shinythemes::shinytheme("yeti"),
    inverse = TRUE,
    id = "navtab",
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
            sensor_panel_ui("test")
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
              value = "main",
              tags$br(),
              shiny::column(
                width= 12,
                overview_tab_ui("test")
              )
            )
          )
        )
      )
    ),
    # Use ShinyJS
    shinyjs::useShinyjs(debug = TRUE)
  )

}
