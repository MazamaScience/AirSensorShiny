comparison_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          leaflet::leafletOutput(
            outputId = ns("comparison_leaflet")
          ) %>% loadSpinner()
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 5,
        tags$h4("Sensor Status"),
        shiny::wellPanel(
          DT::dataTableOutput(
            outputId = ns("sensor_status_table")
          ) %>% loadSpinner(proxy.height = "200px")
        ),

        tags$h4("Sensor-Monitor Correlation"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("sensor_monitor_correlation")
          ) %>% loadSpinner()
        )
      ),

      shiny::column(
        width = 7,
        tags$h4("Sensor-Monitor Comparison"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("sensor_monitor_comparison")
          ) %>% loadSpinner()
        )
      )
    )
  )
}

#' Comparison Module
#'
#' @param input reactive inputs
#' @param output reactive outputs
#' @param session a shiny session
#' @param pat a reactive pat promise object
#' @param sensor a reactive sensor promise object
comparison_mod <- function(input, output, session, pat, sensor) {

  logger.trace("loaded comparison module...")

  # NOTE: An error exists inthe code to where selecting different datetime/lookback
  #       on this tab resets the sensor_picker and breaks the page.
  #       likely has to do with the the order or load
  output$comparison_leaflet <- leaflet::renderLeaflet({
    shiny::req(input$sensor_picker)
    sensor() %...>%
      (function(s) {
        dr <- range(s$data$datetime)
        nearby <- s$meta$pwfsl_closestMonitorID
        dist <- s$meta$pwfsl_closestDistance
        mon <- PWFSLSmoke::monitor_load( monitorIDs = nearby,
                                         startdate = dr[1],
                                         enddate = dr[2] )
        print(nearby)
        shiny_sensorLeaflet(s) %>%
          leaflet::addAwesomeMarkers( lng = mon$meta$longitude,
                                      lat = mon$meta$latitude,
                                      label = "Nearest Regulatory Monitor" ) %>%
          leaflet::addPolylines( lng = c(mon$meta$longitude,s$meta$longitude ),
                                 lat = c(mon$meta$latitude,s$meta$latitude),
                                 label = paste0("Distance: ", signif(dist/1000, 2), " km") )
      }) %...!%
      (function(e) {
        logger.error(e)
        return(NULL)
      })
  })

  output$sensor_monitor_correlation <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    sensor() %...>%
      (function(s) {
        shiny_externalFit(sensor = s, tz = TZ)
      }) %...!%
      (function(e) {
        logger.error(e)
        return(NULL)
      })
  })

  output$sensor_monitor_comparison <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    pat() %...>%
      (function(p) {
        pat_monitorComparison(pat = p)
      }) %...!%
      (function(e) {
        logger.error(e)
        return(NULL)
      })
  })

  output$sensor_status_table <- DT::renderDT({
    shiny::req(input$sensor_picker)
    pat() %...>%
      (function(p) {
        shiny::req(input$sensor_picker)
        DT::datatable(
          shiny_comparisonTable(p),
          selection = "none",
          colnames = "",
          options = list(dom = 't', bSort = FALSE),
          class = 'cell-border stripe'
        ) %>%
          DT::formatRound(columns = 1, digits = 2)
      }) %...!%
      (function(e) {
        logger.error(e)
        return(NULL)
      })
  })

}
