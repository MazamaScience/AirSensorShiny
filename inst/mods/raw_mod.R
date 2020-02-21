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

#' Raw Data Module
#'
#' @param input reactive inputs
#' @param output reactive outputs
#' @param session a shiny session
#' @param pat A reactive pat promise object.
raw_mod <- function(input, output, session, pat) {

  logger.trace("loaded raw data module...")

  # Multiplot
  output$multi_plot <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    pat() %...>%
      (function(p) {
            AirSensor::pat_multiplot(p, columns = 1, timezone = TZ)
      }) %...!% (function(e) NULL)
  })


  #Channel Overlay plot
  output$ch_overlay_plot <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    pat() %...>%
      (function(p) {
            shiny_internalFit(p, whichPlot = 'ab', tz = TZ)
      }) %...!% (function(e) NULL)
  })
  # # Channel Correlation plot
  output$ch_correlation_plot <- shiny::renderPlot({
    shiny::req(input$sensor_picker)
    pat() %...>%
      (function(p) {
            shiny_internalFit(p, whichPlot = 'lm', tz = TZ)
      }) %...!% (function(e) NULL)
  })
}
