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
            outputId = ns("comparison_table")
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

comparison_mod <- function(input, output, session) {
  output$comparison_leaflet <- leaflet::renderLeaflet({
    sensor() %...>%
      ( function(s) {
        dr <- range(s$data$datetime)
        nearby <- s$meta$pwfsl_closestMonitorID
        dist <- s$meta$pwfsl_closestDistance
        mon <- PWFSLSmoke::monitor_load( monitorIDs = nearby,
                                         startdate = dr[1],
                                         enddate = dr[2] )
        print(nearby)
        shiny_sensorLeaflet(s) %>%
          leaflet::addAwesomeMarkers( lng = mon$meta$longitude,
                                      lat = mon$meta$latitude ) %>%
          leaflet::addPolylines( lng = c(mon$meta$longitude,s$meta$longitude ),
                                 lat = c(mon$meta$latitude,s$meta$latitude) )
      } )
  })

  output$sensor_monitor_correlation <- shiny::renderPlot({
    sensor() %...>%
      ( function(s) {
        shiny_externalFit(sensor = s)
      } )
  })

  output$sensor_monitor_comparison <- shiny::renderPlot({
    pat() %...>%
      ( function(p) {
        pat_monitorComparison(pat = p)
      } )
  })

}
