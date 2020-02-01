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
    ) %>% loadSpinner(),
    plotly::plotlyOutput(
      outputId = ns("barplotly"),
      height = 330
    )%>% loadSpinner()
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
    while(!resolved(annual_sensors())) {cat("|")}
    # For coloring the markers based on the date
    annual_sensors() %...>%
      (function(s) {
        tryCatch(
          expr = {
            shiny_sensorLeaflet( sensor = s,
                                 startdate = sd,
                                 enddate = ed,
                                 maptype = "OpenStreetMap")
          },
          error = function (e) {
            logger.error(e)
          },
          finally = print("Annual Sensors Loaded")
        )
      })
  })

  # Plotly barplot output
  output$barplotly <- plotly::renderPlotly({
    shiny::req(input$sensor_picker)
    ed <- lubridate::ymd(input$date_picker)
    sd <- ed - as.numeric(input$lookback_picker)
    while(!resolved(sensor())) {cat("/")}
    sensor() %...>%
      (function(s) {
        tryCatch(
          expr = {
            shiny_barplotly(s, sd, ed)
          },
          error = function(e) {
            logger.error(e)
            PWFSLSmoke::createEmptyMonitor()
            NULL
          }
        )
      })
  })

  # Update leaflet on marker click
  # NOTE: Updates the active sensor
  # NOTE: Adds marker highlight on click
  shiny::observeEvent(
    eventExpr = input$leaflet_marker_click,
    handlerExpr = {
      tryCatch(
        expr = {
          print(input$leaflet_marker_click)
          sensor_label <- input$leaflet_marker_click$id
          leaflet::leafletProxy("leaflet") %>%
            leaflet::addCircleMarkers( lng = input$leaflet_marker_click$lng,
                                       lat = input$leaflet_marker_click$lat,
                                       radius = 10,
                                       fillOpacity = 0.95,
                                       layerId = "tmp",
                                       color = "#ffa020",
                                       options = list(leaflet::pathOptions(interactive = FALSE), bubblingMouseEvents = TRUE) )
          # Handle hightlighted marker re-click
          if ( sensor_label != 'tmp' ) {
            shiny::updateSelectInput(session, "sensor_picker", selected = sensor_label)
          }
        },
        error = function(e) {
          logger.error(e)
        }
      )
    }
  )
  # Update leaflet on sensor pick selection - highlght marker
  shiny::observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {input$sensor_picker; input$date_picker; input$lookback_picker},
    handlerExpr = {
      print("Update leaflet marker from sensor picker")
      while(!resolved(sensor())) {cat("/")}
      sensor() %...>%
        (function(s) {
          tryCatch(
            expr = {
              leaflet::leafletProxy("leaflet") %>%
                leaflet::addCircleMarkers( lng = s$meta$longitude,
                                           lat = s$meta$latitude,
                                           radius = 10,
                                           fillOpacity = 0.95,
                                           layerId = "tmp",
                                           color = "#ffa020",
                                           options = list(leaflet::pathOptions(interactive = FALSE)) )
            },
            error = function(e) {
              logger.error(e)
            }
          )
        })
    }
  )

}
