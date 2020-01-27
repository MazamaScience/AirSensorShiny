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
  shiny::callModule(dataview_mod, "global")

}
