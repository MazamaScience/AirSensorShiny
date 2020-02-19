#' TAB: Overview User Interface
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples

map_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(
      outputId = ns("leaflet"),
    ) %>% loadSpinner()
  )

}

#' TAB: Overview Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
map_mod <- function(input, output, session, annual_sensors, dates) {

  # Leaflet map output
  output$leaflet <- leaflet::renderLeaflet({
    shiny::req(annual_sensors())
    ed <- dates()$ed
    sd <- dates()$sd
    annual_sensors() %...>%
      (function(s) {
        # For coloring the markers based on the date
        shiny_sensorLeaflet( sensor = s,
                             startdate = sd,
                             enddate = ed,
                             maptype = "OpenStreetMap",
                             radius = 9,
                             opacity = 0.95 )
      }) %...!% (function(e) NULL)
  })



  selected <- reactive(input$leaflet_marker_click)
  # Update leaflet on marker click
  # NOTE: Updates the active sensor
  # NOTE: Adds marker highlight on click
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    selected(),
    {
      leaflet::leafletProxy("leaflet", data = selected()) %>%
        leaflet::removeMarker('selected') %>%
        leaflet::addCircleMarkers( lng = ~lng,
                                   lat = ~ lat,
                                   color = '#42434C',
                                   fillColor = '#EABA5E',
                                   fillOpacity = 1,
                                   radius = 9, opacity = 0.95,
                                   weight = 2, layerId  = 'selected')    # Update the selector input with the selected() reactive $id component
      updateSelectInput(session, inputId = 'sensor_picker', selected = selected()$id)

    }
  )

  # # Update leaflet on sensor pick selection - highlight marker
  shiny::observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {input$sensor_picker; input$date_picker; input$lookback_picker},
    handlerExpr = {
      annual_sensors() %...>%
        sensor_filterMeta(monitorID == input$sensor_picker) %...>%
        (function(s) {
          leaflet::leafletProxy("leaflet", data = s$meta) %>%
            leaflet::removeMarker('selected') %>%
            leaflet::addCircleMarkers( lng = ~longitude,
                                       lat = ~latitude,
                                       color = '#42434C',
                                       fillColor = '#EABA5E',
                                       fillOpacity = 1,
                                       radius = 9,
                                       opacity = 1,
                                       weight = 2, layerId = 'selected')
        }) %...!% (function(e) NULL)
    }
  )
}

