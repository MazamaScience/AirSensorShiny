video_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::column(
    width = 12,
    tags$h4("6-Day Sensor Timelapse"),
    shiny::wellPanel(
      shiny::uiOutput(
        outputId = ns("video")
      ) %>% loadSpinner()
    )
  )
}

#' Animation Module
#'
#' @param input reactive inputs
#' @param output reactive outputs
#' @param session a shiny session
#' @param selected_community A reactive selected community object
#' @param dates A reactive dates object
video_mod <- function(input, output, session, selected_community, dates) {

  logger.trace("loaded video module...")

  output$video <- shiny::renderUI({
    shiny::req(input$community_picker)
    if ( selected_community() != "all" ) {
      ed <- dates()$ed
      baseUrl <- "http://smoke.mazamascience.com/data/PurpleAir/videos/"
      year    <- lubridate::year(ed)#strftime(ed, "%Y")
      mm      <- strftime(ed, "%m")
      dd      <- strftime(ed, "%d")
      id    <- com2id(selected_community())
      url <- paste0(baseUrl, year, "/", id, "_", year, mm, dd, ".mp4" )
      tags$video(
        id = "video",
        type = "video/mp4",
        src = url,
        controls = "controls"
      )
    }
  })

}
