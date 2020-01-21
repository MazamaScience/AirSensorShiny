#' Sensor Panel User Interface
#'
#' @param id
sensor_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::pickerInput(
      inputId = ns("community-picker"),
      label = tags$h4("Community"),
      choices = c("All..." = "all", SENSOR_COMMUNITIES),
      options = list(title = "Select community...")
    ),

    shinyWidgets::pickerInput(
      inputId = ns("sensor-picker"),
      label = tags$h4("Sensor"),
      choices = SENSOR_LABELS,
      selected = "",
      options = list(
        `live-search` = TRUE,
        title = "Select sensor...",
        size = 7)
    ),

    shinyWidgets::airDatepickerInput(
      inputId = ns("date-picker"),
      label = tags$h4("Date"),
      value = c(lubridate::now()-lubridate::days(7),
                lubridate::now()),
      todayButton = FALSE,
      addon = "none",
      inline = TRUE,
      separator = " to ",
      range = FALSE,
      #maxDate = lubridate::now(tzone = TIMEZONE),
      minDate = lubridate::ymd(20180102)
    ),

    shinyWidgets::radioGroupButtons(
      inputId = ns("lookback_picker"),
      label = tags$h4("View Past"),
      choices = c( "3 Days" = 3,
                   "7 Days" = 7,
                   "15 Days" = 15,
                   "30 Days" = 30 ),
      justified = T,
      direction = "vertical",
      individual = F,
      checkIcon = list(
        yes = tags$i(class = "fa fa-check",
                     style = "color: #008cba"))

    ),
    shiny::bookmarkButton(
      label = tags$small("Share..."),
      icon = shiny::icon("share-square"),
      title = "Copy Link to Share",
      id = ns("bookmark-button")
    )
  )
}

#' Sensor Panel Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
sensor_panel <- function(input, output, session, active) {

  # Update the active sensor picker choices when sensor labels is updated
  observeEvent(
    eventExpr = active$sensor_labels,
    handlerExpr = {
      shiny::updateSelectInput( session,
                                "sensor-picker",
                                choices = active$sensor_labels )
    }
  )

  # NOTE: ShinyJS is used to identify which input to accept and update from.
  #       This is necessary to remove circular and redundant logic/state.
  # Update the input type on sensor picker mouse enter
  shinyjs::onevent(
    event = "mouseenter",
    id = "sensor-picker",
    expr = {
      active$input_type <- "sensor-picker"
    }
  )

  observeEvent(
    ignoreInit = TRUE,
    eventExpr = input$`sensor-picker`,
    handlerExpr = {
      active$sensor <- pat_createAirSensor(pat_load( input$`sensor-picker`,
                                                     startdate = 20200101,
                                                     enddate = 20200102) )
    }
  )
}


if (F) {
  ui <- shiny::fluidPage(
    left_panel_ui("test")
  )
  server <- function(input, output, session) {
    callModule(left_panel, "test")
  }

  shinyApp(ui, server)
}
