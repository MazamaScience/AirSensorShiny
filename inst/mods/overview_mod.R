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
overview_mod <- function(input, output, session) {

  # Leaflet map output
  output$leaflet <- leaflet::renderLeaflet({
    ed <- lubridate::ymd(input$date_picker)
    sd <- ed - as.numeric(input$lookback_picker)

    # For coloring the markers based on the date
    annual_sensors() %...>%
      shiny_sensorLeaflet( startdate = sd,
                           enddate = ed,
                           maptype = "OpenStreetMap" )

  })
  # Plotly barplot output
  output$barplotly <- plotly::renderPlotly({
    ed <- lubridate::ymd(input$date_picker)
    sd <- ed - as.numeric(input$lookback_picker)
    sensor() %...>%
      shiny_barplotly(sd, ed)
  })

  # Update leaflet on marker click
  # NOTE: Updates the active sensor
  # NOTE: Adds marker highlight on click
  shiny::observeEvent(
    eventExpr = input$leaflet_marker_click,
    handlerExpr = {
      print(input$leaflet_marker_click)
      sensor_label <- input$leaflet_marker_click$id
      leaflet::leafletProxy("leaflet") %>%
        leaflet::addCircleMarkers( lng = input$leaflet_marker_click$lng,
                                   lat = input$leaflet_marker_click$lat,
                                   radius = 10,
                                   fillOpacity = 0.95,
                                   layerId = "selectTmp",
                                   color = "#ffa020",
                                   options = list(leaflet::pathOptions(interactive = FALSE)) )
      shiny::updateSelectInput(session, "sensor_picker", selected = sensor_label)
    }
  )

  shiny::observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {input$sensor_picker; input$date_picker; input$lookback_picker},
    handlerExpr = {
      print("Update leaflet marker from sensor picker")
      sensor() %...>%
        ( function(s) {
          tryCatch(
            expr = {
              leaflet::leafletProxy("leaflet") %>%
                leaflet::addCircleMarkers( lng = s$meta$longitude,
                                           lat = s$meta$latitude,
                                           radius = 10,
                                           fillOpacity = 0.95,
                                           layerId = "selectTmp",
                                           color = "#ffa020",
                                           options = list(leaflet::pathOptions(interactive = FALSE)) )
            },
            error = function(e) {
              e
            }
          )
        } )
    }
  )
}


#######

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
