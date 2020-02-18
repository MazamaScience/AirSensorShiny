#' AirSensor DataViewer Server Logic
#'
#' @param input
#' @param output
#' @param session
MazamaCoreUtils::logger.debug("----- server() ------")
server <- function(input, output, session) {

  selected <- reactiveVal()
  dates <- reactiveVal()
  pat <- reactiveVal()
  year <- reactiveVal()
  annual_sensors <- reactiveVal()
  annual_pat <- reactiveVal()
  noaa <- reactiveVal()

  shiny::callModule(
    panel_mod,
    "global",
    annual_sensors = reactive(annual_sensors())
  )

  shiny::callModule(
    map_mod,
    "global",
    annual_sensors = reactive(annual_sensors()),
    dates = reactive(dates())
  )

  shiny::callModule(
    barplotly_mod,
    "global",
    pat = reactive(pat()),
    dates = reactive(dates())
  )

  shiny::callModule(
    calendar_mod,
    "global",
     annual_pat = reactive(annual_pat())
  )

  shiny::callModule(
    raw_mod,
    "global",
    pat = reactive(pat())
  )

  # Set Sensor Selection on sensor picker update
  observeEvent(
    {
      input$`global-sensor_picker`
    },
    {
      selected(input$`global-sensor_picker`)
      logger.trace(paste0("Selected Sensor: ", selected()))
    }
  )
  # Set Dates on lookback/date picker update
  observeEvent(
    {
      input$`global-date_picker`
      input$`global-lookback_picker`
    },
    {
      ed <- lubridate::ymd(input$`global-date_picker`, tz = TZ) + lubridate::days(1)
      sd <- ed - lubridate::days(as.numeric(input$`global-lookback_picker`)) - lubridate::days(1)
      # df <- data.frame('sd' = as.numeric(strftime(sd, '%Y%m%d', tz = TZ)), 'ed' = as.numeric(strftime(ed, '%Y%m%d', tz = TZ)) )
      df <- data.frame('sd' = sd, 'ed' = ed)
      dates(df)
      logger.trace(paste0("Selected Dates: ", sd, "-", ed))
    }
  )

  observeEvent(
    {
      selected()
      dates()
    },
    {
      label <- selected()
      sd <- dates()$sd
      ed <- dates()$ed
      p <- future({
        setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
        tryCatch(pat_load(label, sd, ed), error = function(e) e)
      })
      pat(p)
      logger.trace(paste0("PAT: ", str(value(pat()))))
    }
  )

  observe({

  })

  observeEvent(
    {
      dates()
    },
    {
      datestamp <- lubridate::year(dates()$ed)
      annual_sensors(
        future({
          sensor_loadYear(datestamp = datestamp)
          })
      )
      logger.trace(paste0("Annual Sensors: ", str(value(annual_sensors()))))
    })

  observeEvent(
    {
      dates()
      selected()
    },
    {

      label <- selected()
      sd <- as.numeric(strftime(dates()$sd, '%Y0101'))
      ed <- as.numeric(strftime(dates()$ed, '%Y1231'))
      annual_pat(
        future({
          setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
          pat_load(label, sd, ed)
        })
      )
    }
  )






  # Reactive SENSOR loading.
  # NOTE: - VIP - (Very Important Program)
  #       Creates the sensor monitor object from the reactive PAT promise context.
  # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  # sensor <<- reactive({
  #   pat() %...>%
  #     (function(p) {
  #       future({
  #         pat_createAirSensor(p)
  #       }) %...!%
  #         (function(e) {
  #           logger.error(paste0("\n Create AirSensor - ERROR"))
  #           return(PWFSLSmoke::createEmptyMonitor())
  #         })
  #     })
  # })
  #
  # # Reactive NOAA loading.
  # # NOTE: Creates the NOAA data from the worldmet package.
  # # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  # noaa <<- reactive({
  #   sensor() %...>%
  #     (function(s) {
  #       future({
  #         sd <- strftime(range(s$data$datetime)[1], "%Y-%m-%d", tz = TZ)
  #         ed <- strftime(range(s$data$datetime)[2], "%Y-%m-%d", tz = TZ)
  #         shiny_getNOAA(s, sd, ed, tz = TZ)
  #       }) %...!%
  #         (function(e) {
  #           logger.error(paste0("\n Download NOAA worldmet - ERROR"))
  #           return(NULL)
  #         })
  #     })
  # })``
  #
  # tab <<- eventReactive(input$tab, input$tab)

  # Module Call
  ## Panel Module: Handles Sensor, Community, Date, Lookback, etc., selection \
  ##               and database loading.
  ## Overview Module:
  ## Calendar Module:
  ## Raw Module:
  ## Pattern Module:
  ## Data View Module:
  # panel <- function(annual_sensors, dates) shiny::callModule(panel_mod, "global", annual_sensors, dates)
  #
  # annual_sensors <- reactive({
  #   yr <- as.numeric(strftime(input$`global-date_picker`, "%Y", tz = TZ))
  #   logger.trace("Load annual sensors: ", yr)
  #   future({
  #     setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
  #     rm_invalid(sensor_loadYear(datestamp = yr))
  #   })
  #   # %...!%
  #   #   (function(e) {
  #   #     logger.error(paste0( "\n Download ANNUAL SENSORS - ERROR:",
  #   #                          "\n Date Selection: ", yr ))
  #   #     shinytoastr::toastr_error("Sensor Unavaliable", position = "bottom-left", showDuration = 0)
  #   #     return(NULL)
  #   #   })
  # })
  #
  # dates <- reactive({
  #   ed <- lubridate::ymd(input$`global-date_picker`, tz = TZ) + lubridate::days(1)
  #   sd <- ed - lubridate::days(as.numeric(input$`global-lookback_picker`)) - lubridate::days(1)
  #   data.frame('sd' = as.numeric(strftime(sd, '%Y%m%d', tz = TZ)), 'ed' = as.numeric(strftime(ed, '%Y%m%d', tz = TZ)) )
  # })
  #
  # observe({
  #    panel(value(annual_sensors()), dates = dates())
  # })
  #
  # overview <- function(annual_sensors, dates) shiny::callModule(overview_mod, "global", annual_sensors, dates)
  # barplotly <- function(pat, dates) shiny::callModule(barplotly_mod, "global", pat, dates)
  # selected <- reactive(input$`global-sensor_picker`)
  #
  # pat <- eventReactive({input$`global-sensor_picker`; input$`global-date_picker`; input$`global-lookback_picker`},{
  #   label <- selected()
  #   sd <- dates()$sd
  #   ed <- dates()$ed
  #
  #
  #     print("Load PAT")
  #     setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
  #     tryCatch(pat_load(label = label, sd, ed), error = function(e) e)
  #
  # })
  #
  # sensor <- reactive({
  #   pat <- value(pat())
  #   pat_createAirSensor(pat)
  # })
  #
  # observe({
  #   overview(value(annual_sensors()), dates())
  # })
  #
  # observe({
  #   barplotly(pat(), dates())
  # })

  # shiny::callModule(overview_mod, "global")
  # shiny::callModule(calendar_mod, "global")
  # shiny::callModule(raw_mod, "global")
  # shiny::callModule(pattern_mod, "global")
  # shiny::callModule(comparison_mod, "global")
  # shiny::callModule(video_mod, "global")
  # shiny::callModule(dataview_mod, "global")
  # shiny::callModule(help_mod, "global")
  # shiny::callModule(latest_mod, "global")

  # ----- Bookmarking -----
  observe({
    # Trigger url update every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmark(
    fun = function(state) {
      state$values$sensor <- input$`global-sensor_picker`
      state$values$community <- input$`global-community_picker`
      state$values$date <- input$`global-date_picker`
      state$values$lookback <- input$`global-lookback_picker`
      state$values$tab <- input$tab
      state$values$page <- input$navbar
    }
  )
  # Update the URL to something reasonable
  onBookmarked(
    fun = function(url) {
      clean_url <- paste0( stringr::str_match(url, "http:(.+)/\\/?")[1],
                           "?",
                           stringr::str_match(url, "_values_(.*)")[1] )
      updateQueryString(clean_url)
      # Update URL for share button copy event
      output$bookmark <- shiny::renderUI({
        rclipboard::rclipButton(
          label = tags$small("Share..."),
          icon = shiny::icon("share-square"),
          inputId = "bookmark_button",
          clipText = clean_url
        )
      })
    }
  )
  # Show "Link Copied!" toastr notification on bookmark button click
  observeEvent(
    eventExpr = input$bookmark_button,
    handlerExpr = {
      shinytoastr::toastr_info("Link Copied!", position = "bottom-left", showDuration = 0)
    }
  )
  # On Restore
  # Note: This updates the values based on the session state saved via the URL and state var
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
      # Restore tab
      shiny::updateTabsetPanel(
        session = session,
        inputId = "tab",
        selected = state$values$tab
      )
      # Restore nav page
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbar",
        selected = state$values$page
      )
    }
  )
  # Show modal asking to select and sensor/community
  # observe( {
  #     # Popup Message for user if NULL selection
  #     if ( input$tab != "overview" & input$tab != "anim" ) {
  #       if ( is.null(input$`global-sensor_picker`) || !shiny::isTruthy(input$`global-sensor_picker`) ) {
  #         shinyWidgets::sendSweetAlert(
  #           session,
  #           title = "Please Select a Sensor",
  #           text = "A valid sensor selection is required to view this tab.",
  #           type = "warning",
  #           closeOnClickOutside = TRUE
  #         )
  #       }
  #     } else if ( input$tab == "anim" ) {
  #       if ( input$`global-community_picker` == "all" || !shiny::isTruthy(input$`global-community_picker`) ) {
  #         shinyWidgets::sendSweetAlert(
  #           session,
  #           title = "Please Select a Community",
  #           text = "A valid community selection is required to view this tab.",
  #           type = "warning",
  #           closeOnClickOutside = TRUE
  #         )
  #       }
  #     }
  #     # Remind the user that their selection is void
  #     # pat() %...>%
  #     #   (function(p) {
  #     #     if ( !pat_isPat(p) ) {
  #     #       shinytoastr::toastr_warning( title = "Oops!",
  #     #                                    message = "Please Select a valid sensor.",
  #     #                                    position = "bottom-left" )
  #     #     }
  #     #   })
  #   }
  # )

  # Handle Element hiding based on valid tabs and page
  observeEvent(
    eventExpr = input$tab,
    handlerExpr = {
      shiny::req(input$tab)
      on <- shinyjs::show
      off <- shinyjs::hide
      logger.trace(paste0("Tab: ", input$tab))
      # NOTE: Necessary to reset the elements...
      on("global-sensor_picker",anim = T)
      on("global-date_picker",anim = T)
      on("global-community_picker",anim = T)
      on("global-lookback_picker", anim = T)
      # Now flip off
      if ( input$tab == "anim" ) off("global-sensor_picker", anim = T)
      if ( input$tab == "calendar" ) off("global-lookback_picker", anim = T)
    }
  )
  observeEvent(
    eventExpr = {input$navbar},
    handlerExpr = {
      shiny::req(input$navbar)
      on <- shinyjs::show
      off <- shinyjs::hide
      logger.trace(paste0("Navbar: ", input$navbar))
      on("global-date_picker")
      on("global-lookback_picker")
      if ( input$navbar == "latest" ) off("global-date_picker", anim = T)
      if (input$navbar == "latest" ) off("global-lookback_picker", anim = T)
    }
  )

}
