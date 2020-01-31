latest_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    plotly::plotlyOutput(
      outputId = ns("latest_plot")
    )
  )
}

latest_mod <- function(input, output, session) {
  output$latest_plot <- plotly::renderPlotly({
      p <- pat_loadLatest(input$sensor_picker)$data

        plotly::plot_ly(p, x= ~datetime, y= ~pm25_A, type = "scatter" )
  })
}
