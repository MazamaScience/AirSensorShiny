#' TAB: Calendar User Interface
#'
#' @param id
calendar_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotly::plotlyOutput(
      outputId = ns("calendar"),
      height = 700
    ) %>% loadSpinner()
  )
}

#' TAB: Calendar Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
calendar_mod <- function(input, output, session, annual_pat) {

  output$calendar <- plotly::renderPlotly({
    shiny::req(input$sensor_picker)

    annual_pat <- value(annual_pat())

        tryCatch(
          expr = {
            shiny_calendarPlot(annual_pat, tz = TZ)
          },
          error = function(e) {
            logger.error(e)
            return(NULL)
          }
        )



  })

}
