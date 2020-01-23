#' Sensor Panel User Interface
#'
#' @param id
panel_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::pickerInput(
      inputId = ns("community_picker"),
      label = tags$h4("Community"),
      choices = c("All..." = "all", SENSOR_COMMUNITIES),
      options = list(title = "Select community...")
    ),

    shinyWidgets::pickerInput(
      inputId = ns("sensor_picker"),
      label = tags$h4("Sensor"),
      choices = SENSOR_LABELS,
      selected = "",
      options = list(
        `live-search` = TRUE,
        title = "Select sensor...",
        size = 7)
    ),

    shinyWidgets::airDatepickerInput(
      inputId = ns("date_picker"),
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
panel_mod <- function(input, output, session, active) {

  # Update the active sensor picker choices when sensor labels is updated
  observeEvent(
    eventExpr = active$sensor_labels,
    handlerExpr = {
      shiny::updateSelectInput( session,
                                "sensor_picker",
                                choices = active$sensor_labels )
    }
  )
  # NOTE: ShinyJS is used to identify which input to accept and update from.
  #       This is necessary to remove circular and redundant logic/state from
  #       the leaflet/sensos picker selection.
  # Update the input type on sensor picker mouse enter
  shinyjs::onevent(
    event = "mouseenter",
    id = "sensor_picker",
    expr = {
      active$input_type <- "sensor_picker"
    }
  )
  # Dates
  observeEvent(
    eventExpr = {input$date_picker; input$lookback_picker},
    handlerExpr =  {
      active$ed <- lubridate::ymd(input$date_picker)
      active$sd <- active$ed - as.numeric(input$lookback_picker)
      print(active$ed)
      print(active$sd)
    }
  )

  # SENSOR PICKER AND LEAFLET LOAD EVENT TRIGGER
  # NOTE: This is the sensor loading event handler.
  # NOTE: V important
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {input$sensor_picker; input$lookback_picker; input$date_picker; input$leaflet_marker_click},
    handlerExpr = {
      shiny::req(active$ed)
      shiny::req(active$input_type)

      tryCatch(
        expr = {
          active$pat <- pat_load( switch(active$input_type, "leaflet" = input$leaflet_marker_click$id, "sensor_picker" = input$sensor_picker),
                                  startdate = active$sd,
                                  enddate = active$ed )
          AirSensor::pat_isPat(active$pat)
          active$sensor <- pat_createAirSensor( active$pat,
                                                period = "1 hour",
                                                qc_algorithm = "hourly_AB_01" )
        },
        error = function(e) {
          handleError(FALSE, e)
          notify()
        }
      ) %>% showLoad()
    }
  )

  # Communities
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {input$community_picker},
    handlerExpr = {
      tryCatch(
        expr = {
          active$sensor_labels <- INIT_SENSORS$meta$monitorID[INIT_SENSORS$meta$communityRegion==input$community_picker]
          active$community <- input$community_picker
          print(input$community_picker)
          print(active$sensor_labels)
          shiny::updateSelectInput( session,
                                    "sensor_picker",
                                    choices = c("All",active$sensor_labels) )

        },
        error = {}
      )
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
