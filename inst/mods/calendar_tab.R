#' TAB: Calendar User Interface
#'
#' @param id
calendar_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotly::plotlyOutput(
      outputId = ns("calendar"),
      height = 700
    )
  )
}

#' TAB: Calendar Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
calendar_tab <- function(input, output, session, active) {

  output$calendar <- plotly::renderPlotly({
    tmp <- pat_load('SCSC_33', 20200101, 20200115)
    shiny_calendarPlot(tmp)
  })
}
