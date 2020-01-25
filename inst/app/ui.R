#' AirSensor DataViewer User Interface
#'
#' @param request
ui <- function(request) {

  shiny::fluidPage(
    # ------ Panel Module -Column ----------------------------------------------
    shiny::column(
      width = 2,
      shiny::wellPanel(
        panel_mod_ui("global")
      )
    ),
    shiny::navbarPage(
      # ------ Nav Bar ---------------------------------------------------------
      title = tags$b("AirSensor DataViewer (Beta)"),
      theme = shinythemes::shinytheme("yeti"),
      inverse = TRUE,
      id = "navbar",
      fluid = TRUE,
      collapsible = TRUE,
      position = "fixed-top",
      windowTitle = "AirSensor Viewer (Beta)",
      # ------ Explore Page ----------------------------------------------------
      shiny::tabPanel(
        title = tags$b("Explore"),
        value = "explore",
        shiny::fluidRow(
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
                  overview_mod_ui("global"),
                  # plotly::plotlyOutput(
                  #   outputId = "barplotly",
                  #   height = 330
                  # )
                )
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
                      calendar_mod_ui("global")
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
                raw_mod_ui("global")
              ),
              # ----- Daily patterns tab -----
              shiny::tabPanel(
                title = tags$b("Daily Patterns"),
                icon = shiny::icon("chart-bar"),
                value = "dp",

                tags$br(),
                pattern_mod_ui("global")
              ),
              # ----- Compare tab -----
              shiny::tabPanel(
                title = tags$b("Compare"),
                icon = shiny::icon("balance-scale"),
                value = "comp",
                tags$br(),
                comparison_mod_ui("global")
              ),
              # ---- Video tab ----
              shiny::tabPanel(
                title = tags$b("Community Timelapse"),
                icon = shiny::icon("file-video"),
                value = "animation",
                tags$br(),
                video_mod_ui("global")
              )
            )
          )
        )
      ),

      #----- View Data Page ----------------------------------------------------
      shiny::tabPanel(
        title = tags$b("View Data"),
        value = "dv",
        shiny::fluidRow(
          dataview_mod_ui("global")

        )
      ),
      # ----- About Page -------------------------------------------------------
      shiny::tabPanel(
        title = tags$b("About"),
        value = "about",
        shiny::fluidRow(
          shiny::column(
            width = 10,
            shiny::includeHTML(file.path(getwd(),"../html/about.html"))
          )
        )
      ),
      # Use ShinyJS
      shinyjs::useShinyjs(debug = TRUE),
      waiter::use_waiter(),

      tags$style(type="text/css", "body {padding-top: 70px;}")
    )
  )
}
