#' TAB: Overview User Interface
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
overview_tab_ui <- function(id) {
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
overview_tab <- function(input, output, session, active) {

  # TMP DEBUG DATES
  dates <- c(20200101, 20200102)

  # Leaflet map output
  output$leaflet <- leaflet::renderLeaflet({
    shiny_sensorLeaflet( sensor = INIT_SENSORS,
                         startdate = dates[1],
                         enddate = dates[2],
                         maptype = "Stamen.TonerLite" )
  })

  # Plotly barplot output
  output$barplotly <- plotly::renderPlotly({
    tryCatch(shiny_barplotly(sensor = active$sensor, dates[1], dates[2]), error = function(e) handleError(FALSE, "HI"))
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

  # Update leaflet ob marker click
  # NOTE: Updates the active sensor
  # NOTE: Adds marker highlight on click
  shiny::observeEvent(
    eventExpr = input$leaflet_marker_click,
    handlerExpr = {
      print(input$leaflet_marker_click)
      sensor_label <- input$leaflet_marker_click$id
      active$sensor <- pat_createAirSensor(pat_load(sensor_label, startdate = dates[1], enddate = dates[2]))
      leaflet::leafletProxy("leaflet") %>%
        leaflet::addCircleMarkers( lng = input$leaflet_marker_click$lng,
                                   lat = input$leaflet_marker_click$lat,
                                   radius = 10,
                                   fillOpacity = 0.95,
                                   layerId = "selectTmp",
                                   color = "#ffa020",
                                   options = list(leaflet::pathOptions(interactive = FALSE)) )
    }
  )

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
