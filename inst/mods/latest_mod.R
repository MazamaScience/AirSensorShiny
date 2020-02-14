latest_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    plotly::plotlyOutput(
      outputId = ns("latest_pm")
    ) %>% loadSpinner(),
    shiny::column(
      width = 6,
      plotly::plotlyOutput(
        outputId = ns("latest_rh")
      ) %>% loadSpinner()
    ),
    shiny::column(
      width = 6,
      plotly::plotlyOutput(
        outputId = ns("latest_temp")
      ) %>% loadSpinner()
    )
  )
}

latest_mod <- function(input, output, session) {

  latest <- eventReactive(
    eventExpr = {input$sensor_picker},
    valueExpr = {
      shiny::req(input$sensor_picker)
      label <- input$sensor_picker
      future({
        pat_createNew(pas = PAS, label = label)
      }) %...!%
        (function(e) {
          logger.error(paste0( "\n Download LATEST PAT - ERROR:",
                               "\n Input Selection: ", label ))
          shinytoastr::toastr_error( title = "Oops! Sensor Unavaliable.",
                                     message = "Please try a different sensor.",
                                     position = "bottom-left",
                                     showDuration = 0 )
          return(NULL)
        })
    })

  output$latest_pm <- plotly::renderPlotly({
    latest() %...>%
      (function(p) {
        tryCatch(
          expr = {
            p$data$datetime <- lubridate::with_tz(p$data$datetime, tzone = TZ)
            plotly::plot_ly( p$data,
                             x = ~datetime,
                             y = ~pm25_A,
                             type = "scatter",
                             mode = "lines",
                             line = list(color ="red"),#"#ba2e00"),
                             name = "Channel A",
                             opacity = 0.65 ) %>%
              plotly::add_trace( y = ~pm25_B,
                                 line = list(color = "blue"),#"#008cba"),
                                 name = "Channel B" ) %>%
              plotly::config(displayModeBar = FALSE) %>%
              plotly::layout( title = list(text = paste0(p$meta$label, " Latest Data")),
                              legend = list(orientation = 'h'),
                              xaxis = list(title = "Date", titlefont = list(size = 14.5)),
                              yaxis = list(title = "PM<sub>2.5</sub> (\u03bcg / m\u00b3)", titlefont = list(size = 14.5)) )
          },
          error = function(e) {
            logger.error(e)
            return(NULL)
          }
        )
      })
  })

  output$latest_rh <- plotly::renderPlotly({
    latest() %...>%
      (function(p) {
        tryCatch(
          expr = {
            p$data$datetime <- lubridate::with_tz(p$data$datetime, tzone = TZ)
            plotly::plot_ly( p$data,
                             x = ~datetime,
                             y = ~humidity,
                             type = "scatter",
                             mode = "lines",
                             line = list(color ="black"),
                             opacity = 0.65 ) %>%
              plotly::config(displayModeBar = FALSE) %>%
              plotly::layout( title = list(text = "Humidity"),
                              xaxis = list(title = "Date", titlefont = list(size = 14.5)),
                              yaxis = list(title = "RH (%)", titlefont = list(size = 14.5)) )
          },
          error = function(e) {
            logger.error(e)
            return(NULL)
          }
        )
      })
  })

  output$latest_temp <- plotly::renderPlotly({
    latest() %...>%
      (function(p) {
        tryCatch(
          expr = {
            p$data$datetime <- lubridate::with_tz(p$data$datetime, tzone = TZ)
            plotly::plot_ly( p$data,
                             x = ~datetime,
                             y = ~temperature,
                             type = "scatter",
                             mode = "lines",
                             line = list(color ="black"),
                             opacity = 0.65 ) %>%
              plotly::config(displayModeBar = FALSE) %>%
              plotly::layout( title = list(text = "Temperature"),
                              xaxis = list(title = "Date", titlefont = list(size = 14.5)),
                              yaxis = list(title = "Temperature (F)", titlefont = list(size = 14.5)) )
          },
          error = function(e) {
            logger.error(e)
            return(NULL)
          }
        )
      })
  })

}
