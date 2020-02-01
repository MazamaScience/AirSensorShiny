help_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::prettyToggle(
      status_on = "success",
      status_off = "primary",
      inputId = ns("help_button"),
      label_on = "",
      label_off = "",
      outline = TRUE,
      plain = TRUE,
      icon_on = icon("question-circle", class = "regular"),
      icon_off = icon("question-circle", class = "solid")
    ),
    shiny::uiOutput(
      outputId = ns("help")
    )
  )
}

help_mod <- function(input, output, session) {
  # React to help button toggle, returns T/F
  help <- eventReactive(input$help_button, input$help_button)
  output$help <- shiny::renderUI({
    # Switch to different HTML documents depending on current view
    if ( help() ) {
      switch(
        tab(),
        "overview" = {shiny::includeHTML(file.path(getwd(),"../www/overview_help.html"))},
        "calendar" = {shiny::includeHTML(file.path(getwd(),"../www/calendar_help.html"))},
        "raw" = {shiny::includeHTML(file.path(getwd(),"../www/raw_help.html"))},
        "dp" = {shiny::includeHTML(file.path(getwd(),"../www/dp_help.html"))},
        "comp" = {shiny::includeHTML(file.path(getwd(),"../www/comparison_help.html"))},
        "anim" = {shiny::includeHTML(file.path(getwd(),"../www/animation_help.html"))}
      )
    }
  })
}
