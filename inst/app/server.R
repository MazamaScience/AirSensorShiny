#' AirSensor DataViewer Server Logic
#'
#' @param input
#' @param output
#' @param session
server <- function(input, output, session) {

  # Instantiate global reactive values
  active <- shiny::reactiveValues( sensor = NULL,
                                   sensor_labels = NULL,
                                   input_type = NULL )
  # Module Call
  # NOTE: "test" for development
  shiny::callModule(overview_tab, "test", active)
  shiny::callModule(sensor_panel, "test", active)

  # Update the downstream sensor functions
  # NOTE: Updates the leaflet marker or sensor picker determined by the JS event
  #       handler -- i.e. mouse events.
  shiny::observeEvent(
    eventExpr = active$sensor,
    handlerExpr = {
      print(active$input_type) # DEBUG
      switch( EXPR = active$input_type,
              "leaflet" = {
                shiny::updateSelectInput(
                  session,
                  "test-sensor-picker",
                  selected = active$sensor$meta$monitorID )
              },
              "sensor-picker" = {
                leaflet::addCircleMarkers(
                  map = leaflet::leafletProxy("test-leaflet"),
                  lng = active$sensor$meta$longitude,
                  lat = active$sensor$meta$latitude,
                  radius = 10,
                  fillOpacity = 0.95,
                  layerId = "selectTmp",
                  color = "#ffa020",
                  options = list(leaflet::pathOptions(interactive = FALSE)) )
              } )
    }
  )
}
