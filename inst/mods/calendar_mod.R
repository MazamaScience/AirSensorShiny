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
calendar_mod <- function(input, output, session) {

  output$calendar <- plotly::renderPlotly({
    shiny::req(input$sensor_picker)
    while(!resolved(annual_pat())) {cat(".")}
    annual_pat() %...>%
      (function(s) {
        tryCatch(
          expr = {
            shiny_calendarPlot(s, tz = TZ)
          },
          error = function(e) {
            logger.error(e)
            return(NULL)
          }
        )
      })


  })

}
