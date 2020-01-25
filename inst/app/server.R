#' AirSensor DataViewer Server Logic
#'
#' @param input
#' @param output
#' @param session
server <- function(input, output, session) {


  library(future)
  library(promises)
  plan(multiprocess)


  pat <<- eventReactive(
    eventExpr = {
      input$`explore-sensor_picker`; input$`explore-date_picker`; input$`explore-lookback_picker`
    },
    valueExpr = {
      label <- input$`explore-sensor_picker`
      ed <- lubridate::ymd(input$`explore-date_picker`)
      sd <- ed - as.numeric(input$`explore-lookback_picker`)
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
            print(e)
          }
        )
      })
    }
  )

  annual_pat <<- eventReactive(
    eventExpr = {
      input$`explore-sensor_picker`; input$`explore-date_picker`; input$`explore-lookback_picker`
    },
    valueExpr = {
      label <- input$`explore-sensor_picker`
      print("load annual pat")
      yr <- as.numeric(strftime(input$`explore-date_picker`, "%Y"))
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

  annual_sensors <<- eventReactive(
    eventExpr = {
      input$`explore-date-picker`; input$`explore-lookback_picker`
    },
    valueExpr = {
      tmp <- as.numeric(strftime(input$`explore-date_picker`, "%Y"))
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

  sensor <<- reactive({
    tryCatch(
      expr = {
        pat() %...>% pat_createAirSensor()
      },
      error = function(e) print(e)
    )
  })

  shiny::callModule(panel_mod,"explore")
  shiny::callModule(overview_mod, "explore")
  shiny::callModule(calendar_mod, "explore")
  shiny::callModule(raw_mod, "explore")
  shiny::callModule(pattern_mod, "explore")


}
