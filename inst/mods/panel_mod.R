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
panel_mod <- function(input, output, session, annual_sensors, dates) {

  # dates <- eventReactive(
  #   eventExpr = {
  #     input$date_picker; input$lookback_picker
  #   },
  #   valueExpr = {
  #     ed <- lubridate::ymd(input$date_picker, tz = TZ) + lubridate::days(1)
  #     sd <- ed - lubridate::days(as.numeric(input$lookback_picker)) - lubridate::days(1)
  #     data.frame('sd' = as.numeric(strftime(sd, '%Y%m%d', tz = TZ)), 'ed' = as.numeric(strftime(ed, '%Y%m%d', tz = TZ)) )
  #   }
  # )
  #
  # # Reactive PAT loading handler.
  # # NOTE: - VIP -
  # #       Downloads the PAT in range of selected date picker and lookback from
  # #       the set archive server, global.
  # # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  # pat <- eventReactive(
  #   ignoreNULL = TRUE,
  #   eventExpr = {
  #     input$sensor_picker; input$date_picker; input$lookback_picker
  #   },
  #   valueExpr = {
  #     shiny::req(input$sensor_picker)
  #     label <- input$sensor_picker
  #     ed <- dates()$ed
  #     sd <- dates()$sd
  #     future({
  #       setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
  #
  #       pat_load( label,
  #                 startdate = sd,
  #                 enddate = ed )
  #     }) %...!%
  #       (function(e) {
  #         logger.error(paste0( "\n Download PAT - ERROR:",
  #                              "\n Input Selection: ", label,
  #                              "\n Date Selection: ", sd, "-", ed ))
  #         # shinytoastr::toastr_error( title = "Oops! Sensor Unavaliable.",
  #         #                            message = "Please try a different sensor or date.",
  #         #                            position = "bottom-left",
  #         #                            showDuration = 0 )
  #         shinyjs::runjs("if($('#dem').hasClass('in')) {$('#collapse_btn').click();} else {$('#collapse_btn').click();};")
  #         return(NULL)
  #       })
  #   }
  # )
  #
  # # Reactive Annual PAT loading handler.
  # # NOTE: - VIP -
  # #       Downloads the annual PAT object from the selected the input date
  # #       picker year stamp, global.
  # # NOTE: Asynchronous following Future/Promise protocol to reduce concurrent
  # #       event call cost.
  # annual_pat <- eventReactive(
  #   eventExpr = {
  #     input$sensor_picker;# input$date_picker; input$lookback_picker
  #   },
  #   valueExpr = {
  #     label <- input$sensor_picker
  #     logger.trace(paste0("load annual pat: ",label))
  #     yr <- as.numeric(strftime(input$date_picker, "%Y", tz = TZ))
  #     ed <- paste0(yr, "1231")
  #     sd <- paste0(yr,"0101")
  #     logger.trace(paste(label, as.numeric(stringr::str_remove_all(sd, "-")), as.numeric(stringr::str_remove_all(ed, "-")) ,sep= "-"))
  #     future({
  #       setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
  #       pat_load( label,
  #                 startdate = as.numeric(stringr::str_remove_all(sd, "-")),
  #                 enddate = as.numeric(stringr::str_remove_all(ed, "-")) )
  #     }) %...!%
  #       (function(e) {
  #         logger.error(paste0( "\n Downlaod ANNUAL PAT - ERROR:",
  #                              "\n Input Selection: ", label,
  #                              "\n Date Selection: ", sd, "-", ed ))
  #         shinytoastr::toastr_error("Sensor Unavaliable", position = "bottom-left", showDuration = 0)
  #         return(NULL)
  #       })
  #   }
  # )
  #
  # # Reactive Annual SENSOR loading handler.
  # # NOTE: - VIP -
  # #       Downloads the annual sensor monitor object from the selected the input
  # #       date picker year stamp, global.
  # # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  # annual_sensors <- eventReactive(
  #   ignoreNULL = TRUE,
  #   eventExpr = {
  #     input$date_picker
  #   },
  #   valueExpr = {
  #     yr <- as.numeric(strftime(input$date_picker, "%Y", tz = TZ))
  #     logger.trace("Load annual sensors: ", yr)
  #     future({
  #       setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
  #       rm_invalid(sensor_loadYear(datestamp = yr))
  #     }) %...!%
  #       (function(e) {
  #         logger.error(paste0( "\n Download ANNUAL SENSORS - ERROR:",
  #                              "\n Date Selection: ", yr ))
  #         shinytoastr::toastr_error("Sensor Unavaliable", position = "bottom-left", showDuration = 0)
  #         return(NULL)
  #       })
  #   }
  # )
  # NOTE: Avoid erroneous sensor selection options by keeping the map options
  #       and picker options identical.
  observe({
    annual_sensors %...>%
      (function(s) {
        shinyWidgets::updatePickerInput(
          session,
          "sensor_picker",
          choices = unique(s$meta$monitorID)
        )
      })
  })

  # Handle Downloads
  output$download <- shiny::downloadHandler(
    filename = function() {
      label <- input$sensor_picker
      ed <- dates$ed
      sd <- dates$sd
      paste0(label,sd,"_",ed,".csv")
    },
    content = function(file) {
      label <- input$sensor_picker
      ed <- dates$ed
      sd <- dates$sd
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

  # Community Selection Event Handler
  # NOTE: Handles the input$community picker
  # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {input$community_picker; input$lookback_picker; input$date_picker},
    handlerExpr = {
      annual_sensors %...>%
        (function(s) {
          tryCatch(
            expr = {
              # Calculate the selected community location
              if ( input$community_picker == 'all' ) {
                community_sensors <- s$meta
              } else {
                community_sensors <- s$meta[id2com(s$meta$communityRegion) == input$community_picker,]
              }
              bbox <- lapply( community_sensors[c('longitude', 'latitude')],
                              function(x) c(min = min(x), max = max(x)) )
              # Change leaflet bounds to community
              leaflet::leafletProxy('leaflet') %>%
                leaflet::fitBounds( lng1 = bbox$longitude[[1]],
                                    lng2 = bbox$longitude[[2]],
                                    lat1 = bbox$latitude[[1]],
                                    lat2 = bbox$latitude[[2]] )
              # May be useful
              not_community_sensors <- !(s$meta$monitorID %in% community_sensors$monitorID)
              # determine what sort of map update to do
              com <- unique(id2com(s$meta$communityRegion))
              if ( input$community_picker != "all" ) {
                # Only show community group
                leaflet::leafletProxy("leaflet") %>%
                  leaflet::showGroup(group = com[com == input$community_picker]) %>%
                  leaflet::hideGroup(group = com[com != input$community_picker]) %>%
                  leaflet::removeMarker(layerId = "tmp") # Remove the old marker selection
                # Restrict the available sensors to selected community
                shinyWidgets::updatePickerInput( session = session,
                                                 inputId = 'sensor_picker',
                                                 choices = community_sensors$monitorID,
                                                 selected = input$sensor_picker )
              } else {
                # Show all community groups
                leaflet::leafletProxy("leaflet") %>%
                  leaflet::showGroup(group = com) %>%
                  leaflet::removeMarker(layerId = "tmp") # Remove old marker selection
                # Redraw selected marker
                shinyWidgets::updatePickerInput( session = session,
                                                 inputId = "sensor_picker",
                                                 choices = community_sensors$monitorID,
                                                 selected = input$sensor_picker)
              }
            },
            error = function(e) {
              logger.error(paste0("Error in community pick: ", e))
            }
          )
        })
    }
  )
}
