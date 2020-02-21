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


#' Calendar Module
#'
#' @param input reactive inputs
#' @param output reactive outputs
#' @param session a shiny session
#' @param annual_pat a reactive annual pat promise object
calendar_mod <- function(input, output, session, annual_pat) {

  logger.trace("loaded calendar module...")

  output$calendar <- plotly::renderPlotly({
    shiny::req(annual_pat())

    annual_pat() %...>%
      (function(p) {
        shiny_calendarPlot(p, tz = TZ)
      }) %...!%
      (function(e) {
        logger.error(e)
        return(NULL)
      })
  })

}
