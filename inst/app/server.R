#' AirSensor DataViewer Server Logic
#'
#' @param input
#' @param output
#' @param session
MazamaCoreUtils::logger.debug("----- server() ------")
server <- function(input, output, session) {

  # ------ Module Call ------
  shiny::callModule( module = panel_mod,
                     id = "global",
                     annual_sensors = reactive(annual_sensors()),
                     selected_sensor = reactive(selected_sensor()),
                     selected_community = reactive(selected_community()),
                     dates = reactive(dates()) )

  shiny::callModule( module = map_mod,
                     id = "global",
                     annual_sensors = reactive(annual_sensors()),
                     dates = reactive(dates()),
                     selected_sensor = reactive(selected_sensor()),
                     selected_community = reactive(selected_community()) )

  shiny::callModule( module = barplotly_mod,
                     id = "global",
                     sensor = reactive(sensor()),
                     dates = reactive(dates()) )

  shiny::callModule( module = calendar_mod,
                     id = "global",
                     annual_pat = reactive(annual_pat()) )

  shiny::callModule( module = raw_mod,
                     id = "global",
                     pat = reactive(pat()) )

  shiny::callModule( module = pattern_mod,
                     id = "global",
                     sensor = reactive(sensor()),
                     noaa = reactive(noaa()) )

  shiny::callModule( module = comparison_mod,
                     id = "global",
                     pat = reactive(pat()),
                     sensor = reactive(sensor()) )

  shiny::callModule( module = video_mod,
                     id = "global",
                     selected_community = reactive(selected_community()),
                     dates = reactive(dates()) )

  shiny::callModule( module = dataview_mod,
                     id = "global",
                     pat = reactive(pat()) )

  shiny::callModule( module = latest_mod,
                     id = "global" )

  shiny::callModule( module = help_mod,
                     id = "global",
                     current_tab = reactive(current_tab()) )

  # ------ Reactive Expressions ------
  # Set Sensor Selection on sensor picker update
  selected_sensor <- reactiveVal()
  observe({
    selected_sensor(input$`global-sensor_picker`)
    logger.trace(paste0("Selected Sensor: ", selected_sensor()))
  })

  # Set Dates on lookback/date picker update
  dates <- reactiveVal()
  observe({
    ed <- lubridate::ymd(input$`global-date_picker`, tz = TZ) + lubridate::days(1)
    sd <- ed - lubridate::days(as.numeric(input$`global-lookback_picker`)) - lubridate::days(1)
    dates(data.frame('sd' = sd, 'ed' = ed))
    logger.trace(paste0("Selected Dates: ", sd, "-", ed))
  })

  # Set the PAT
  pat <- reactiveVal()
  observe({
    label <- selected_sensor()
    sd <- dates()$sd
    ed <- dates()$ed
    pat(
      future({
        setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
        pat_load(label, sd, ed)
      }) %...!%
        (function(e) {
          return(NULL)
        })
    )
  })

  # Set the Sensor
  sensor <- reactiveVal()
  observe({
    pat() %...>%
      (function(p) {
        sensor(
          future({
            pat_createAirSensor(p)
          }) %...!% (function(e) NULL)
        )
      })
  })

  # Set the selected year
  year <- reactiveVal(lubridate::year(lubridate::now()))
  observe({
    if ( lubridate::year(dates()$ed) != year() ) {
      year(lubridate::year(dates()$ed))
      logger.trace(paste0("Year Change: ", year()))
    }
  })

  # Set annual sensors
  annual_sensors <- reactiveVal()
  observeEvent(
    eventExpr = {
      year()
    },
    handlerExpr = {
      datestamp <- year()
      annual_sensors(
        future({
          setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
          rm_invalid(sensor_loadYear(datestamp = datestamp))
        }) %...!% (function(e) NULL)
      )
      logger.trace('Set Annual Sensors')

    })

  # Set annual PAT
  annual_pat <- reactiveVal()
  observeEvent(
    eventExpr = {
      year()
      selected_sensor()
    },
    handlerExpr = {
      label <- selected_sensor()
      sd <- as.numeric(paste0(year(), '0101'))
      ed <- as.numeric(paste0(year(), '1231'))
      annual_pat(
        future({
          setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
          pat_load(label, sd, ed)
        }) %...!% (function(e) NULL)
      )
      logger.trace('Set Annual PAT')
    }
  )

  # Set NOAA met data
  noaa <- reactiveVal()
  observeEvent(
    eventExpr = {
      sensor()
    },
    handlerExpr = {
      sd <- dates()$sd
      ed <- dates()$ed
      sensor() %...>%
        (function(s) {
          noaa(
            future({
              shiny_getNOAA(s, sd, ed, tz = TZ)
            }) %...!% (function(e) NULL)
          )
        })
    }
  )

  # Set Community selection
  selected_community <- reactiveVal()
  observe({
      selected_community(input$`global-community_picker`)
      logger.trace(paste0("Selected Community: ", selected_community()))
  })

  # Set current tab
  current_tab <- reactiveVal()
  observe({
      current_tab(input$tab)
  })

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

  # ----- Error, UI, and Modal Handling ------
  # Show modal asking to select and sensor/community
  observe({
    # Popup Message for user if NULL selection
    if ( input$tab != "overview" & input$tab != "anim" ) {
      if ( is.null(input$`global-sensor_picker`) || !shiny::isTruthy(input$`global-sensor_picker`) ) {
        shinyWidgets::sendSweetAlert(
          session,
          title = "Please Select a Sensor",
          text = "A valid sensor selection is required to view this tab.",
          type = "info",
          closeOnClickOutside = TRUE
        )
      }
    } else if ( input$tab == "anim" ) {
      if ( input$`global-community_picker` == "all" || !shiny::isTruthy(input$`global-community_picker`) ) {
        shinyWidgets::sendSweetAlert(
          session,
          title = "Please Select a Community",
          text = "A valid community selection is required to view this tab.",
          type = "info",
          closeOnClickOutside = TRUE
        )
      }
    }
  })

  # Show Invalid PAT modal
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {
      selected_sensor()
    },
    handlerExpr = {
      if ( isTruthy(selected_sensor()) ) {
      # Remind the user that their selection is void
      pat() %...>%
        (function(p) {
          if ( !pat_isPat(p) ) {
            shinytoastr::toastr_warning( title = "Oops!",
                                         message = "Something wrent wrong. Please select a different sensor or date.",
                                         position = "bottom-left" )
          }
        })
      }
    }
  )

  # Show Invalid PAT modal
  observeEvent(
    ignoreInit = TRUE,
    ignoreNULL = FALSE,
    eventExpr = {
      sensor()
    },
    handlerExpr = {
      # Remind the user that their selection is void
      sensor() %...>%
        (function(s) {
          if ( !sensor_isSensor(s) || sensor_isEmpty(s) ) {
            shinyjs::runjs("if($('#dem').hasClass('in')) {$('#collapse_btn').click();};")
          }
        })
    }
  )

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
