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
    tryCatch(
      expr = {
        while(!resolved(annual_pat())) {cat(".")}
        annual_pat() %...>% shiny_calendarPlot()
      },
      error = function(e) {
        handleError(FALSE, notify("Internal Error"))
      }
    )

  })

}
