pattern_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::wellPanel(
      shiny::plotOutput(
        outputId = ns("daily_pattern_plot")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 5,

        tags$h4("Additional NOAA Weather Data"),
        shiny::wellPanel(
          DT::dataTableOutput(
            outputId = ns("noaa_table")
          ) %>% loadSpinner()
        )
      ),
      shiny::column(
        width = 7,
        tags$h4("Wind Rose Plot"),
        shiny::wellPanel(
          shiny::plotOutput(
            outputId = ns("wind_plot")
          ) %>% loadSpinner()
        )
      )
    )
  )
}

pattern_mod <- function(input, output, session, active) {

  active$noaa <-  eventReactive(active$sensor, shiny_getNOAA(active$sensor, active$sd, active$ed))

  output$daily_pattern_plot <- shiny::renderPlot({
    sd <- as.numeric(stringr::str_remove_all(active$sd, '-'))
    ed <- as.numeric(stringr::str_remove_all(active$ed, '-'))
    shiny::req(active$sensor, active$ed, active$sd)
    shiny_diurnalPattern(active$sensor, startdate = sd, enddate = ed)
  })
  output$noaa_table <- shiny::renderDataTable({

  })
}
