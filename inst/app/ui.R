#' AirSensor DataViewer User Interface
#'
#' @param request
MazamaCoreUtils::logger.debug("------ ui() ------")
ui <- function(request) {
  shiny::fluidPage(
    # ------ Panel Module -Column ----------------------------------------------
    shiny::column(
      width = 2,
      shiny::wellPanel(
        id = "panel",
        panel_mod_ui("global")
      ),
      shiny::tags$footer(id = "ver", paste0("Version: ", VERSION))
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
      windowTitle = "AirSensor DataViewer",
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
              id = "tab",
              # ---- Overview Tab ----
              shiny::tabPanel(
                title = tags$b("Overview"),
                icon = shiny::icon("map-marked-alt"),
                value = "overview",
                tags$br(),
                shiny::column(
                  width= 12,
                  overview_mod_ui("global"),
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
                value = "anim",
                tags$br(),
                video_mod_ui("global")
              )
            )
          ),
          # HELP
          shiny::column(
            width = 2,
            help_mod_ui("global")

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
      shiny::tabPanel(
        title = tags$b("Latest Data"),
        value = "latest",
        shiny::fluidRow(
          shiny::column(
            width = 10,
            latest_mod_ui("global")
          )
        )
      ),
      # ----- About Page -------------------------------------------------------
      shiny::tabPanel(
        title = tags$b("About"),
        value = "about",
        shiny::fluidRow(
          shiny::column(
            width = 10,
            shiny::includeHTML(file.path(getwd(),"../www/about.html"))
          )
        )
      )
    ),

    # Use ShinyJS
    shinyjs::useShinyjs(debug = TRUE),
    rclipboard::rclipboardSetup(),
    shinytoastr::useToastr(),

    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tags$style(type="text/css", "footer {padding-left: 5%; color: #808080; font-size: 11px}"),
    tags$style(type="text/css", ".well {background-color: #fff}"),
    tags$style(type="text/css", "#panel {min-height: 600px; min-width:236px;}"),
    tags$head(
      tags$script(
        'var dimension = [0, 0];
         $(document).on("shiny:connected", function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });'
      )
    ),
  )
}
