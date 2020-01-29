#' AirSensor DataViewer Server Logic
#'
#' @param input
#' @param output
#' @param session
server <- function(input, output, session) {

  # Reactive SENSOR loading.
  # NOTE: - VIP - (Very Important Program)
  #       Creates the sensor monitor object from the reactive PAT promise context.
  # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  sensor <<- reactive({
    pat() %...>%
      ( function(p) {
        tryCatch(
          expr = {
            pat_createAirSensor(p)
          },
          error = function(e) {
            print("ERROR creating Sensor object")
          }
        )
      } )
  })

  noaa <<- reactive({
    sensor() %...>%
      ( function(s) {
        future({
          sd <- strftime(range(s$data$datetime)[1], "%Y-%m-%d")
          ed <- strftime(range(s$data$datetime)[2], "%Y-%m-%d")

          shiny_getNOAA(s, sd, ed)
        })
      } )
  })

  # Module Call
  ## Panel Module: Handles Sensor, Community, Date, Lookback, etc., selection \
  ##               and database loading.
  ## Overview Module:
  ## Calendar Module:
  ## Raw Module:
  ## Pattern Module:
  ## Data View Module:
  shiny::callModule(panel_mod,"global")
  shiny::callModule(overview_mod, "global")
  shiny::callModule(calendar_mod, "global")
  shiny::callModule(raw_mod, "global")
  shiny::callModule(pattern_mod, "global")
  shiny::callModule(comparison_mod, "global")
  shiny::callModule(video_mod, "global")
  shiny::callModule(dataview_mod, "global")

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmark(
    fun = function (state) {
    state$values$sensor <- input$`global-sensor_picker`
    state$values$community <- input$`global-community_picker`
    state$values$date <- input$`global-date_picker`
    state$values$lookback <- input$`global-lookback_picker`
    state$values$tab <- input$tab_select
  })
  onBookmarked(
    fun = function(url) {
    updateQueryString(
      paste0( stringr::str_match(url, "http:(.+)/\\/?")[1],
              "?",
              stringr::str_match(url, "_values_(.*)")[1] )
    )
  })
  shiny::onRestored(
    fun = function(state) {

      # restore the panel selections
      shinyWidgets::updatePickerInput(
        session,
        inputId = "global-sensor_picker",
        selected = state$values$sensor
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = "global-community_picker",
        selected = state$values$community
      )
      shinyWidgets::updateAirDateInput(
        session,
        inputId = "global-date_picker",
        value = state$values$date
      )
      shinyWidgets::updateRadioGroupButtons(
        session,
        inputId = "global-lookback_picker",
        selected = state$values$lookback
      )
    }
  )




}
