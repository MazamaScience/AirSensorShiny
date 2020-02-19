pattern_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::wellPanel(
      shiny::plotOutput(
        outputId = ns("daily_pattern_plot")
      ) %>% loadSpinner()
    ),
    shiny::fluidRow(
      shiny::column(
        width = 5,

        tags$h4("Additional NOAA Weather Data"),
        shiny::wellPanel(
          DT::dataTableOutput(
            outputId = ns("noaa_table")
          ) %>% loadSpinner()
        )
      ),
      shiny::column(
        width = 7,
        tags$h4("Wind Rose Plot"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("wind_plot")
          ) %>% loadSpinner()
        )
      )
    )
  )
}

pattern_mod <- function(input, output, session, sensor, noaa) {

  output$daily_pattern_plot <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    sensor() %...>%
      (function(s) {
            AirMonitorPlots::ggplot_pm25Diurnal( ws_data = s,
                                                 offsetBreaks = TRUE ) +
              AirMonitorPlots::stat_meanByHour(output = "scaqmd")
      }) %...!% (function(e) NULL)
  })
  output$noaa_table <- DT::renderDT({
    shiny::req(input$sensor_picker)
    noaa() %...>%
      (function(n) {
            data <- shiny_noaa_table(n)
            DT::datatable(
              data = data,
              selection = "none",
              colnames = "",
              options = list(dom = 't', bSort = FALSE),
              class = 'cell-border stripe'
            ) %>%
              DT::formatRound(columns = 1, digits = 2)
      }) %...!% (function(e) NULL)
  })

  # NOTE: Look into finding a better method than "nesting" promises
  output$wind_plot  <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    sensor() %...>%
      (function(s) {
            noaa() %...>%
          (function(n) {
                AirSensor::sensor_pollutionRose(s, n)
          })
      }) %...!% (function(e) NULL)
  })

}
