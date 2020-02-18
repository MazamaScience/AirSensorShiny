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
    # shiny::absolutePanel(
    #   id = "plot_panel",
    #   fixed = FALSE,
    #   left = "auto",
    #   right = "auto",
    #   bottom = 0,
    #   width = "100%",
    #   height = "inherit",
    #   # Show/Hide barplot panel button
    #   HTML('<a id = "collapse_btn" class = "collapsed" data-toggle="collapse" data-target="#dem" style="margin-left:50%;">
    #        <span class="glyphicon glyphicon-chevron-up"></span> Select a Sensor</a>'),
    #   # Put barplot in "dem" html
    #   tags$div(
    #     id = 'dem',
    #     class = "collapse",
    #     plotly::plotlyOutput(
    #       outputId = ns("barplotly"),
    #       height = 300
    #     ) %>% loadSpinner(),
    #   )
    # ),
    # # Barplot panel opacity CSS and leaflet padding fix
    # tags$style(
    #   type = "text/css",
    #   '#plot_panel{
    #     /* Appearance */
    #     background-color: white;
    #     padding: 0 0 0 0;
    #     cursor: move;
    #     /* Fade out while not hovering */
    #     opacity: 0.70;
    #     zoom: 0.9;
    #     transition: opacity 300ms 500ms;
    #   }
    #   #plot_panel:hover {
    #     /* Fade in while hovering */
    #     opacity: 1;
    #     transition-delay: 0;
    #   }
    #   .col-sm-12{
    #     padding: 0 0 0 0;
    #   }'
    # )
  )

}

#' TAB: Overview Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
map_mod <- function(input, output, session, annual_sensors, dates) {

  # annual_sensors <- reactiveVal(value(annual_sensors))
  # dates <- reactiveVal(dates)

  # Leaflet map output
  output$leaflet <- leaflet::renderLeaflet({
    ed <- dates()$ed
    sd <- dates()$sd
    annual_sensors <- value(annual_sensors())
    # For coloring the markers based on the date
        tryCatch(
          expr = {
            shiny_sensorLeaflet( sensor = annual_sensors,
                                 startdate = sd,
                                 enddate = ed,
                                 maptype = "OpenStreetMap",
                                 radius = 9,
                                 opacity = 0.95 )
          },
          error = function (e) {
            logger.error(e)
          },
          finally = print("Annual Sensors Loaded")
        )
  })

  # # Plotly barplot output
  # output$barplotly <- plotly::renderPlotly({
  #   shiny::req(input$sensor_picker)
  #   ed <-  dates$ed
  #   sd <- dates$sd
  #   # while(!resolved(sensor)) {cat("/")}
  #       tryCatch(
  #         expr = {
  #           sensor <- pat_createAirSensor(pat)
  #           shiny_barplotly(sensor, sd, ed, tz = TZ)  %>%
  #             # Hacky JS way to change the cursor back to normal pointer
  #             htmlwidgets::onRender(
  #               "function(el, x) {
  #                 Plotly.d3.select('.cursor-ew-resize').style('cursor', 'default')
  #               }"
  #             )
  #         },
  #         error = function(e) {
  #           logger.error(e)
  #           PWFSLSmoke::createEmptyMonitor()
  #           NULL
  #         }
  #       )
  # })

  selected <- reactive(input$leaflet_marker_click)
  # Update leaflet on marker click
  # NOTE: Updates the active sensor
  # NOTE: Adds marker highlight on click
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    selected(),
    {
      library(leaflet)
      leafletProxy("leaflet", data = selected()) %>%
        removeMarker('selected') %>%
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


  # shiny::observeEvent(
  #   eventExpr = input$leaflet_marker_click,
  #   handlerExpr = {
  #     tryCatch(
  #       expr = {
  #         print(input$leaflet_marker_click)
  #         sensor_label <- input$leaflet_marker_click$id
  #         leaflet::leafletProxy("leaflet", data = input$leaflet_marker_click) %>%
  #           leaflet::addCircleMarkers( lng = ~lng,
  #                                      lat = ~ lat,
  #                                      color = '#42434C',
  #                                      fillColor = '#EABA5E',
  #                                      fillOpacity = 1,
  #                                      radius = 9, opacity = 0.95,
  #                                      weight = 2, layerId  = 'tmp')
  #         # Handle hightlighted marker re-click
  #         if ( sensor_label != 'tmp' ) {
  #           shiny::updateSelectInput(session, "sensor_picker", selected = sensor_label)
  #         }
  #       },
  #       error = function(e) {
  #         logger.error(e)
  #       }
  #     )
  #   }
  # )

  # # Update leaflet on sensor pick selection - highlight marker
  shiny::observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {input$sensor_picker; input$date_picker; input$lookback_picker},
    handlerExpr = {
      sensor <- sensor_filterMeta(value(annual_sensors()), monitorID == input$sensor_picker)
      print("Update leaflet marker from sensor picker")
          tryCatch(
            expr = {
              leaflet::leafletProxy("leaflet", data = sensor$meta) %>%
                leaflet::addCircleMarkers( lng = ~longitude,
                                           lat = ~latitude,
                                           color = '#42434C',
                                           fillColor = '#EABA5E',
                                           fillOpacity = 1,
                                           radius = 9,
                                           opacity = 1,
                                           weight = 2, layerId = 'tmp')
            },
            error = function(e) {
              logger.error(e)
            }
          )
    }
  )
}

