#' Sensor Panel User Interface
#'
#' @param id
#' @export
panel_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::pickerInput(
      inputId = ns("community_picker"),
      label = tags$h4("Community"),
      choices = c("All..." = "all",
                  id2com(SENSOR_COMMUNITIES)),
      selected = "all"
    ),

    shinyWidgets::pickerInput(
      inputId = ns("sensor_picker"),
      label = tags$h4("Sensor"),
      choices = SENSOR_LABELS,
      selected = "",
      options = list(
        `live-search` = TRUE,
        title = "Select sensor...",
        size = 7 )
    ),

    shinyWidgets::airDatepickerInput(
      inputId = ns("date_picker"),
      label = tags$h4("Date"),
      value = c(lubridate::now(tzone = TZ)-lubridate::days(7),
                lubridate::now(tzone = TZ)),
      todayButton = TRUE,
      addon = "none",
      inline = FALSE,
      separator = " to ",
      range = FALSE,
      maxDate = lubridate::now(tzone = TZ),
      minDate = lubridate::ymd(20180102, tz = TZ)
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
    # Download and Share
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::downloadButton(ns("download"), label = tags$small("Download"))
      ),
      shiny::column(
        width = 6,
        shiny::uiOutput("bookmark")
      )
    ),
    tags$style(
      type = "text/css",
      '#global-download{
        width: 100%;
      }
      #bookmark_button{
        width: 100%;
      }'
    )
  )
}

#' Sensor Panel Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
panel_mod <- function(input, output, session, annual_sensors, selected_sensor, selected_community, dates) {

  # Handle Downloads
  output$download <- shiny::downloadHandler(
    filename = function() {
      label <- selected_sensor()
      ed <- dates()$ed
      sd <- dates()$sd
      paste0(label,sd,"_",ed,".csv")
    },
    content = function(file) {
      label <- selected_sensor()
      ed <- dates()$ed
      sd <- dates()$sd
      tryCatch(
        expr = {
          p <- AirSensor::pat_load(label, sd, ed)
          write.csv(p$data[1:5], file = file)
        },
        error = function(e) {
          logger.error(e)
          shinytoastr::toastr_error( title = "Oops! Download Failed.",
                                     message = "Try a different date or sensor.",
                                     position = "bottom-left" )
          return(write.csv(NULL))
        }
      )
    }
  )

  observeEvent(
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    eventExpr = {selected_sensor()},
    handlerExpr = {
      if ( isTruthy(selected_sensor())) {
        shinyjs::runjs("if(!$('#dem').hasClass('in')) {$('#collapse_btn').click();};")
      } else {
        shinyjs::runjs("if($('#dem').hasClass('in')) {$('#collapse_btn').click();};")
      }
    }
  )

  observeEvent({selected_community(); annual_sensors()}, {
    annual_sensors() %...>%
      (function(s) {
        if ( selected_community() != 'all' ) {
          sensors <- s$meta[com2id(s$meta$communityRegion) == com2id(selected_community()),]
          shinyWidgets::updatePickerInput(
            session,
            inputId = "sensor_picker",
            choices = sensors$monitorID
          )
        } else {
          shinyWidgets::updatePickerInput(
            session,
            inputId = "sensor_picker",
            choices = s$meta$monitorID,
            selected = selected_sensor()
          )
        }
      }) %...!% (function(e) NULL)
  })

}
