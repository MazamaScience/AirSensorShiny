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
            outputId = ns("raw_plot"),
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
            outputId = ns("channels_plot")
          ) %>% loadSpinner()
        )
      ),
      shiny::column(
        width = 6,
        tags$h4("Channel Correlation"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("correlation_plot")
          ) %>% loadSpinner()
        )
      )
    )
  )
}

raw_mod <- function(input, output, session, active) {

  output$raw_plot <- shiny::renderCachedPlot({
    shiny::req(active$pat)

    handleError(
      AirSensor::pat_isPat(active$pat),
      "Please select a sensor."
    )

    multiPlot <- AirSensor::pat_multiplot(active$pat, columns = 2)

    return(multiPlot)

  }, cacheKeyExpr = list(active$ed, active$ed, active$sensor$meta$monitorID))


}
