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
    shiny::req(input$sensor_picker)
    sensor() %...>%
      (function(s) {
        tryCatch(
          expr = {
            AirMonitorPlots::ggplot_pm25Diurnal( ws_data = s,
                                                 offsetBreaks = TRUE ) +
              AirMonitorPlots::stat_meanByHour(output = "scaqmd")
          },
          error = function(e) {
            logger.error(e)
            return(NULL)
          }
        )
      })
  })
  output$noaa_table <- DT::renderDT({
    shiny::req(input$sensor_picker)
    noaa() %...>%
      (function(n) {
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
          error = function(e) {
            logger.error(e)
            return(NULL)
          }
        )
      })
  })

  # NOTE: Look into finding a better method than "nesting" promises
  output$wind_plot  <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    sensor() %...>%
      (function(s) {
            noaa() %...>% ( function(n) {
              tryCatch(
                expr = AirSensor::sensor_pollutionRose(s, n),
                error = function(e) {
                  shinytoastr::toastr_warning(
                    title = "Wind Rose Plot Error",
                    message = "Wind Rose Plot failed to render. Please try a different sensor or date.",
                    position = "bottom-left"
                  )
                  logger.error(e)
                  return(NULL)
                }
              )
            })
      })
  })

}
