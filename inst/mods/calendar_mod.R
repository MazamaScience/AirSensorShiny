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
calendar_mod <- function(input, output, session, active) {

  output$calendar <- plotly::renderPlotly({
    tryCatch(
      expr = {
        tmp <-pat_load( active$sensor$meta$monitorID,
                        startdate = as.numeric(paste0(active$year, "0101")),
                        enddate = as.numeric(paste0(active$year+1, "0101")) )
        shiny_calendarPlot(tmp)
      },
      error = function(e) {
        handleError(FALSE, notify("Internal Error"))
      }
    )

  })

}
