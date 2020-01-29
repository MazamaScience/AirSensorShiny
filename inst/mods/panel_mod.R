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
      inline = FALSE,
      separator = " to ",
      range = FALSE,
      maxDate = lubridate::now(),
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

    )
  )
}

#' Sensor Panel Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
panel_mod <- function(input, output, session) {

  # Reactive PAT loading handler.
  # NOTE: - VIP -
  #       Downloads the PAT in range of selected date picker and lookback from
  #       the set archive server, global.
  # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  pat <<- eventReactive(
    eventExpr = {
      input$sensor_picker; input$date_picker; input$lookback_picker
    },
    valueExpr = {
      label <- input$sensor_picker
      ed <- lubridate::ymd(input$date_picker)
      sd <- ed - as.numeric(input$lookback_picker)
      print(paste(label, as.numeric(stringr::str_remove_all(sd, "-")), as.numeric(stringr::str_remove_all(ed, "-")) ,sep= "-"))
      future({
        setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
        tryCatch(
          expr = {
            pat_load( label,
                      startdate = as.numeric(stringr::str_remove_all(sd, "-")),
                      enddate = as.numeric(stringr::str_remove_all(ed, "-")) )
          },
          error = function(e) {
            e
          }
        )
      })
    }
  )

  # Reactive Annual PAT loading handler.
  # NOTE: - VIP -
  #       Downloads the annual PAT object from the selected the input date
  #       picker year stamp, global.
  # NOTE: Asynchronous following Future/Promise protocol to reduce concurrent
  #       event call cost.
  annual_pat <<- eventReactive(
    eventExpr = {
      input$sensor_picker;# input$date_picker; input$lookback_picker
    },
    valueExpr = {
      label <- input$sensor_picker
      print("load annual pat")
      yr <- as.numeric(strftime(input$date_picker, "%Y"))
      ed <- paste0(yr, "1231")
      sd <- paste0(yr,"0101")
      print(paste(label, as.numeric(stringr::str_remove_all(sd, "-")), as.numeric(stringr::str_remove_all(ed, "-")) ,sep= "-"))
      future({
        setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
        tryCatch(
          expr = {
            pat_load( label,
                      startdate = as.numeric(stringr::str_remove_all(sd, "-")),
                      enddate = as.numeric(stringr::str_remove_all(ed, "-")) )
          }, error = function(e) print(e) )
      })
    }
  )
  # Reactive Annual SENSOR loading handler.
  # NOTE: - VIP -
  #       Downloads the annual sensor monitor object from the selected the input
  #       date picker year stamp, global.
  # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  annual_sensors <<- eventReactive(
    eventExpr = {
      input$date_picker; input$lookback_picker
    },
    valueExpr = {
      tmp <- as.numeric(strftime(input$date_picker, "%Y"))
      paste0("load annual sensors: ", tmp)
      future({
        setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
        tryCatch(
          expr = {
            sensor_loadYear(datestamp = tmp )
          },
          error = function(e) {e}
        )
      })
    }
  )

  # Community Selection Event Handler
  # NOTE: Handles the input$community picker
  # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {input$community_picker},
    handlerExpr = {
      shinyWidgets::updatePickerInput(session = session, inputId = 'sensor_picker', selected = NULL)
      annual_sensors() %...>%
        ( function(s) {
          tryCatch(
            expr = {
              # Calculate the selected community location
              if ( grepl("[aA]ll", input$community_picker) ) {
                community_sensors <- s$meta
              } else {
                community_sensors <- s$meta[s$meta$communityRegion == input$community_picker,]
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

              # TODO: Restrict the available sensors to selected community
              # NOTE: updating the selection to only have the selected community's
              #       sensors throws an error when selecting a sensor outside of
              #       the community sensors.
              shinyWidgets::updatePickerInput( session = session,
                                               inputId = 'sensor_picker',
                                               choices = s$meta$monitorID )

            },
            error = function(e) {print("Error in community pick")}
          )
        } )
    }
  )

}


if (F) {

  # Promises Tester
  library(future)
  library(promises)
  plan(multiprocess)
  ui <- shiny::fluidPage(
    panel_mod_ui("test"),
    shiny::textOutput("result")
  )
  server <- function(input, output, session) {
    active <- shiny::reactiveValues( sensor = NULL,
                                     label_sensors = NULL,
                                     input_type = "sensor_picker",
                                     year = as.numeric(strftime(Sys.time(), "%Y")),
                                     ed = NULL,
                                     sd = NULL,
                                     days = NULL,
                                     annual_sensors = NULL,
                                     community = NULL,
                                     pat = NULL,
                                     meta_sensors = NULL )

    s <- reactiveVal(NULL)
    m <- reactiveVal(NULL)

    callModule(panel_mod, "test", active)

    output$result <- shiny::renderText(str(s()))

    observeEvent(input$sensor_picker, {

      label <- input$`sensor_picker`
      future({
        setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
        pat_load(label, 20190101, 20200101)
        }) %...>% s
      print("DOING STUFF")

    future({
      setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
      pat_load(label, 20190101, 20200101) %>% pat_createAirSensor()
    }) %...>% m
    print("DOING OTHER STUFF")
  })


  }

  shinyApp(ui, server)
}
