dataview_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # # Meta explorer
    shiny::column(
      width = 9,
      shiny::tableOutput(
        outputId = ns("meta_table")
      )
    ),
    # Data explorer
    shiny::column(
      width = 10,
      DT::dataTableOutput(
        outputId = ns("data_table")
      ) %>% loadSpinner()
    )
  )
}

dataview_mod <- function(input, output, session, pat) {

  output$meta_table <- shiny::renderTable({
    shiny::req(input$sensor_picker)
    pat() %...>%
      (function(p) {
            data.frame( "Sensor" = p$meta$label,
                        "Community" = p$meta$communityRegion,
                        "Sensor Type" = p$meta$sensorType,
                        "Longitude" = p$meta$longitude,
                        "Latitude" = p$meta$latitude,
                        "State" = p$meta$stateCode,
                        "Country" = p$meta$countryCode )
      }) %...!% (function(e) NULL)
  })

  output$data_table <- DT::renderDataTable({
    shiny::req(input$sensor_picker)
    memory_debug("Data Explorer")
    # Remove unnecessary columns
    pat() %...>%
      (function(p) {
            data <- p$data[-(6:10)]
            names(data) <- c( "Datetime (UTC)",
                              "PM2.5 Ch. A (\u03bcg / m\u00b)",
                              "PM2.5 Ch. B (\u03bcg / m\u00b)",
                              "Temperature (F)",
                              "Relative Humidity (%)" )

            DT::datatable(data, selection = "none", options = list(pageLength = 25) ) %>%
              DT::formatDate(1, method = 'toLocaleString', params = list('en-EN'))
      }) %...!% (function(e) NULL)
  })

}
