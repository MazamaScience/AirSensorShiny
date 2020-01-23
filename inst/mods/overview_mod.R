#' TAB: Overview User Interface
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
overview_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(
      outputId = ns("leaflet"),
      height = 420
    ),
    plotly::plotlyOutput(
      outputId = ns("barplotly"),
      height = 330
    )
  )
}

#' TAB: Overview Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
#'
#' @return
#' @export
#'
#' @examples
overview_mod <- function(input, output, session, active) {
  # Leaflet map output
  output$leaflet <- leaflet::renderLeaflet({
    shiny::req(active$annual_sensors)
    tryCatch(
      expr = {
      # For coloring the markers based on the date
      shiny_sensorLeaflet( sensor = active$annual_sensors,
                           startdate = active$sd,
                           enddate = active$ed,
                           maptype = "OpenStreetMap" )
      },
      error = function(e) {
      notify()
      })
  })
  # Plotly barplot output
  output$barplotly <- plotly::renderPlotly({
    shiny::req(active$sensor)
    tryCatch(
      expr = {
        shiny_barplotly(sensor = active$sensor, active$sd, active$ed)
      },
      error = function(e) {
        handleError(FALSE, print(e))
      }
    )
  })
  # NOTE: ShinyJS is used to identify which input to accept and update from.
  #       This is necessary to remove circular and redundant logic/state.
  # Update the input type on leaflet mouse enter
  shinyjs::onevent(
    event = "mouseenter",
    id = "leaflet",
    expr = {
      active$input_type <- "leaflet"
    }
  )
  ## NOTE: This is now handled in panel module!
  # Update leaflet on marker click
  # NOTE: Updates the active sensor
  # NOTE: Adds marker highlight on click
  # shiny::observeEvent(
  #   eventExpr = input$leaflet_marker_click,
  #   handlerExpr = {
  #     print(input$leaflet_marker_click)
  #     sensor_label <- input$leaflet_marker_click$id
  #
  #     tryCatch(
  #       expr = {
  #         active$pat <- pat_load( sensor_label,
  #                                 startdate = active$sd,
  #                                 enddate = active$ed )
  #         print(str(active$pat))
  #         active$sensor <- pat_createAirSensor( active$pat,
  #                                               period = "1 hour",
  #                                               qc_algorithm = "hourly_AB_01" )
  #       },
  #       error = function(e) {
  #         handleError(FALSE, notify(paste0(input$leaflet_marker_click$id, ": Unavaliable.")))
  #       }
  #     ) %>% showLoad()
  #     leaflet::leafletProxy("leaflet") %>%
  #       leaflet::addCircleMarkers( lng = input$leaflet_marker_click$lng,
  #                                  lat = input$leaflet_marker_click$lat,
  #                                  radius = 10,
  #                                  fillOpacity = 0.95,
  #                                  layerId = "selectTmp",
  #                                  color = "#ffa020",
  #                                  options = list(leaflet::pathOptions(interactive = FALSE)) )
  #   }
  # )
  shiny::observe({
    # Show/hide barplot
    if ( active$sensor == "" || is.null(active$sensor) ) {
      shinyjs::hide("barplotly",anim = FALSE)
    } else {
      shinyjs::show("barplotly", anim = TRUE, time = 0.25)
    }
  })

}

# Debug As an individual Shiny Application
if (F) {
  ui <- shiny::fluidPage(
    overview_tab_ui("test")
  )
  server <- function(input, output, session) {
    callModule(overview_tab, "test")
  }

  shinyApp(ui, server)
}
