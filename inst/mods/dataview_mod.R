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

dataview_mod <- function(input, output, session, active) {

  output$meta_table <- shiny::renderTable({
    req(active$pat)
    observeEvent(input$sensor_picker, print("HIT"))

    meta <- data.frame( "Sensor" = active$pat$meta$label,
                        "Community" = active$pat$meta$communityRegion,
                        "Sensor Type" = active$pat$meta$sensorType,
                        "Longitude" = active$pat$meta$longitude,
                        "Latitude" = active$pat$meta$latitude,
                        "State" = active$pat$meta$stateCode,
                        "Country" = active$pat$meta$countryCode,
                        "Timezone" = active$pat$meta$timezone )
    return(meta)
  })

  output$data_table <- DT::renderDataTable({
    shiny::req(active$pat)
    memory_debug("Data Explorer")
    # Remove unecessary columns
    data <- active$pat$data[-(6:10)]
    names(data) <- c( "Datetime",
                      "PM2.5 Ch. A (\u03bcg / m\u00b)",
                      "PM2.5 Ch. B (\u03bcg / m\u00b)",
                      "Temperature (F)",
                      "Relative Humidity (%)" )
    data <-
      DT::datatable(data, selection = "none") %>%
      DT::formatDate(1, method = 'toLocaleString', params = list('en-EN'))
    return(data)
  })
}
