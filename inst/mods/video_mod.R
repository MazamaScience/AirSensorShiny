video_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::column(
    width = 12,
    tags$h4("5-Day Sensor Timelapse"),
    shiny::wellPanel(
      shiny::uiOutput(
        outputId = ns("video_out")
      ) %>% loadSpinner()
    )
  )
}
