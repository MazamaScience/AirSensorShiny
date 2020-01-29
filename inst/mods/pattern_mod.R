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
  output$noaa_table <- DT::renderDT({
    noaa() %...>%
      ( function(n) {
      tryCatch(
        expr = {
          data <- shiny_noaa_table(n)
          DT::datatable(
            data = data,
            selection = "none",
            colnames = "",
            options = list(dom = 't', bSort = FALSE),
            class = 'cell-border stripe'
          ) %>%
            DT::formatRound(columns = 1, digits = 2)
        },
        error = function(e) {}
      )
    } )
  })

  # NOTE: Look into finding a better method than "nesting" promises
  output$wind_plot  <- shiny::renderPlot({
     sensor() %...>%
      ( function(s) {
        tryCatch(
          expr = {
            noaa() %...>% ( function(n) AirSensor::sensor_pollutionRose(s, n) )
          },
          error = function(e) {
            e
          }
        )
      } )
  })

}
