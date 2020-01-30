#' TAB: Raw Data Module User Interface
#'
#' @param id
raw_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        height = "800",
        tags$h4("Raw Data"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("multi_plot"),
            height = "800"
          ) %>% loadSpinner()
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        tags$h4("Channel Overlay"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("ch_overlay_plot")
          ) %>% loadSpinner()
        )
      ),
      shiny::column(
        width = 6,
        tags$h4("Channel Correlation"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("ch_correlation_plot")
          ) %>% loadSpinner()
        )
      )
    )
  )
}

#' TAB: Raw Data Module Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
raw_mod <- function(input, output, session) {

  # Multiplot
  output$multi_plot <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    tryCatch(
      expr = {
        pat() %...>% AirSensor::pat_multiplot(columns = 2)
      },
      error = function(e) {
        logger.error(e)
      }
    )
  })
  #Channel Overlay plot
  output$ch_overlay_plot <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    tryCatch(
      expr = {
        pat() %...>% shiny_internalFit(whichPlot = 'ab')
      },
      error = function(e) {
        logger.error(e)
      }
    )
  })
  # # Channel Correlation plot
  output$ch_correlation_plot <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    tryCatch(
      expr = {
        pat() %...>% shiny_internalFit(whichPlot = 'lm')
      },
      error = function(e) {
        logger.error(e)
      }
    )
  })
}
