barplotly_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::absolutePanel(
      id = "plot_panel",
      fixed = FALSE,
      left = "auto",
      right = "auto",
      bottom = 0,
      width = "100%",
      height = "inherit",
      # Show/Hide barplot panel button
      HTML('<a id = "collapse_btn" class = "collapsed" data-toggle="collapse" data-target="#dem" style="margin-left:50%;">
           <span class="glyphicon glyphicon-chevron-up"></span> Select a Sensor</a>'),
      # Put barplot in "dem" html
      tags$div(
        id = 'dem',
        class = "collapse",
        plotly::plotlyOutput(
          outputId = ns("barplotly"),
          height = 300
        ) %>% loadSpinner(),
      )
    ),
    # Barplot panel opacity CSS and leaflet padding fix
    tags$style(
      type = "text/css",
      '#plot_panel{
        /* Appearance */
        background-color: white;
        padding: 0 0 0 0;
        cursor: move;
        /* Fade out while not hovering */
        opacity: 0.70;
        zoom: 0.9;
        transition: opacity 300ms 500ms;
      }
      #plot_panel:hover {
        /* Fade in while hovering */
        opacity: 1;
        transition-delay: 0;
      }
      .col-sm-12{
        padding: 0 0 0 0;
      }'
    )
  )
}

barplotly_mod <- function(input, output, session, sensor, dates) {

  #Plotly barplot output
  output$barplotly <- plotly::renderPlotly({
    shiny::req(input$sensor_picker)
    ed <-  dates()$ed
    sd <- dates()$sd
    sensor() %...>%
      (function(s) {
        shiny_barplotly(s, sd, ed, tz = TZ)  %>%
          # Hacky JS way to change the cursor back to normal pointer
          htmlwidgets::onRender(
            "function(el, x) {
                  Plotly.d3.select('.cursor-ew-resize').style('cursor', 'default')
                }"
          )
      }) %...!% (function(e) NULL)
  })
  # NOTE: Both events below run the JS code to determine if "dem" html is up or down.
  #       If a sensor is selected, and it is not already up, it is pushed up.
  #       If a community is selected, and it is already up, it is pushed down.
  observeEvent(ignoreInit = TRUE, {input$sensor_picker},{
    shinyjs::runjs("if(!$('#dem').hasClass('in')) {$('#collapse_btn').click();};")
  })
  observeEvent({input$community_picker},{
    shinyjs::runjs("if($('#dem').hasClass('in')) {$('#collapse_btn').click();};")
  })

}
