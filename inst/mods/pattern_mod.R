pattern_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::wellPanel(
      shiny::plotOutput(
        outputId = ns("daily_pattern_plot")
      )
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

pattern_mod <- function(input, output, session) {

  output$daily_pattern_plot <- shiny::renderPlot({
    sensor() %...>%
      ( function(s) {
        tryCatch(
          expr = {
            AirMonitorPlots::ggplot_pm25Diurnal( ws_data = s,
                                                 offsetBreaks = TRUE ) +
              AirMonitorPlots::stat_meanByHour(output = "scaqmd")
          },
          error = function(e) {}
        )
      } )
  })
  # output$noaa_table <- shiny::renderDataTable({
  #   tryCatch(
  #     expr = {
  #       shiny::req(active$sensor, active$ed)
  #       active$noaa <- shiny_getNOAA(active$sensor, active$sd, active$ed)
  #       print(active$noaa)
  #       DT::datatable(active$noaa)
  #     },
  #     error = function(e) {}
  #   )
  # })

  output$wind_plot  <- shiny::renderPlot({
    sensor() %...>%
      ( function(s) {
        tryCatch(
          expr = {
            # wind <- shiny_getNOAA(s) %>% dplyr::select(c("date", "wd", "ws"))
            # AirSensor::sensor_pollutionRose(s)
            "WOOSH"
          },
          error = function(e) {
          }
        )
      } )
  })

}
