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
panel_mod <- function(input, output, session) {
  # Communities
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {input$community_picker},
    handlerExpr = {
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
            },
            error = {}
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

    observeEvent(input$`test-sensor_picker`, {

      label <- input$`test-sensor_picker`
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
