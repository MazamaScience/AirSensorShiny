video_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::column(
    width = 12,
    tags$h4("5-Day Sensor Timelapse"),
    shiny::wellPanel(
      shiny::uiOutput(
        outputId = ns("video")
      ) %>% loadSpinner()
    )
  )
}

video_mod <- function(input, output, session) {
  output$video <- shiny::renderUI({
    ed <- input$date_picker

    baseUrl <- "http://smoke.mazamascience.com/data/PurpleAir/videos/"
    year    <- strftime(ed, "%Y")
    mm      <- strftime(ed, "%m")
    dd      <- strftime(ed, "%d")
    comm    <- input$community_picker

    url <- paste0(baseUrl, year, "/", comm, "_", year, mm, dd, ".mp4" )

    tags$video(
      id = "video",
      type = "video/mp4",
      src = url,
      controls = "controls"
    )

  })
}
