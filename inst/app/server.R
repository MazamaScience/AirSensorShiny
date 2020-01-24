#' AirSensor DataViewer Server Logic
#'
#' @param input
#' @param output
#' @param session
server <- function(input, output, session) {

  # Instantiate global reactive values
  active <- shiny::reactiveValues( sensor = NULL,
                                   label_sensors = NULL,
                                   input_type = NULL,
                                   year = as.numeric(strftime(Sys.time(), "%Y")),
                                   ed = NULL,
                                   sd = NULL,
                                   days = NULL,
                                   annual_sensors = NULL,
                                   community = NULL,
                                   pat = NULL,
                                   meta_sensors = NULL )
  # Module Call
  # NOTE: "test" for development
  shiny::callModule(overview_mod, "explore", active)
  shiny::callModule(panel_mod, "explore", active)
  shiny::callModule(calendar_mod, "explore", active)
  shiny::callModule(raw_mod, "explore", active)
  # shiny::callModule(panel_mod,"dv", active)
  shiny::callModule(dataview_mod, "dv", active)
  shiny::callModule(pattern_mod, "explore", active)



  # Initialization annual sensor load
  # NOTE: This action is only preformed on the startup after loading the System's
  #       year datestamp.
  observeEvent(
    once = TRUE,
    eventExpr = active$year,
    handlerExpr = {
      active$annual_sensors <- AirSensor::sensor_loadYear(datestamp = active$year)
      active$meta_sensors <- active$annual_sensors$meta
    }
  )
  # Year Logic
  year <- eventReactive(active$sd, as.numeric(strftime(active$sd, "%Y")))
  observe({
    if (year() != active$year) {
      print("YEAR CHANGE")
      active$year <- year()
      active$annual_sensors <- AirSensor::sensor_loadYear(datestamp = active$year)
    }
  })

  # Update the downstream sensor functions
  # NOTE: Updates the leaflet marker or sensor picker determined by the JS event
  #       handler -- i.e. mouse events.
  shiny::observeEvent(
    eventExpr = {active$sensor; active$pat},
    handlerExpr = {
      print(active$input_type) # DEBUG
      shiny::req(active$input_type)
      switch( EXPR = active$input_type,
              "leaflet" = {
                shiny::updateSelectInput(
                  session,
                  "explore-sensor_picker",
                  selected = active$sensor$meta$monitorID
                )
                shiny::updateSelectInput(
                  session,
                  "dv-sensor_picker",
                  selected = active$sensor$meta$monitorID
                )
              },
              "sensor_picker" = {
                leaflet::addCircleMarkers(
                  map = leaflet::leafletProxy("explore-leaflet"),
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
