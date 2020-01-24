comparison_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::wellPanel(
          leaflet::leafletOutput(
            outputId = ns("comparison_leaflet")
          ) %>% loadSpinner()
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 5,
        tags$h4("Sensor Status"),
        shiny::wellPanel(
          DT::dataTableOutput(
            outputId = ns("comparison_table")
          ) %>% loadSpinner(proxy.height = "200px")
        ),

        tags$h4("Sensor-Monitor Correlation"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = "ws_ext"
          ) %>% loadSpinner()
        )
      ),

      shiny::column(
        width = 7,
        tags$h4("Sensor-Monitor Comparison"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("sensor_monitor_comparison")
          ) %>% loadSpinner()
        )
      )
    )
  )
}

comparison_mod <- function(input, output, session, active) {

}
