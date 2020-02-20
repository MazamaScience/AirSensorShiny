#' TAB: Overview User Interface
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples

map_mod_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(
      outputId = ns("leaflet"),
    ) %>% loadSpinner()
  )

}

#' TAB: Overview Logic
#'
#' @param input
#' @param output
#' @param session
#' @param active
map_mod <- function(input, output, session, annual_sensors, dates, selected_sensor, selected_community) {

  # Leaflet map output
  output$leaflet <- leaflet::renderLeaflet({
    shiny::req(annual_sensors())
    ed <- dates()$ed
    sd <- dates()$sd
    annual_sensors() %...>%
      (function(s) {
        # For coloring the markers based on the date
        shiny_sensorLeaflet( sensor = s,
                             startdate = sd,
                             enddate = ed,
                             maptype = "OpenStreetMap",
                             radius = 9,
                             opacity = 0.95 )
      }) %...!% (function(e) NULL)
  })



  selected_marker <- reactive(input$leaflet_marker_click)
  # Update leaflet on marker click
  # NOTE: Updates the active sensor
  # NOTE: Adds marker highlight on click
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {selected_marker()},
    {
      leaflet::leafletProxy("leaflet", data = selected_marker()) %>%
        leaflet::removeMarker('selected') %>%
        leaflet::addCircleMarkers( lng = ~lng,
                                   lat = ~ lat,
                                   color = '#42434C',
                                   fillColor = '#EABA5E',
                                   fillOpacity = 1,
                                   radius = 9, opacity = 0.95,
                                   weight = 2, layerId  = 'selected')    # Update the selector input with the selected() reactive $id component
      updateSelectInput(session, inputId = 'sensor_picker', selected = selected_marker()$id)

    }
  )

  # # Update leaflet on sensor pick selection - highlight marker
  shiny::observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {selected_sensor()},
    handlerExpr = {
      annual_sensors() %...>%
        sensor_filterMeta(monitorID == selected_sensor()) %...>%
        (function(s) {
          leaflet::leafletProxy("leaflet", data = s$meta) %>%
            leaflet::removeMarker('selected') %>%
            leaflet::addCircleMarkers( lng = ~longitude,
                                       lat = ~latitude,
                                       color = '#42434C',
                                       fillColor = '#EABA5E',
                                       fillOpacity = 1,
                                       radius = 9,
                                       opacity = 1,
                                       weight = 2, layerId = 'selected')
        }) %...!% (function(e) NULL)
    }
  )

  # Community Selection Event Handler
  # NOTE: Handles the input$community picker
  # NOTE: Asynchronous Future/Promise protocol to reduce concurrent event call cost.
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {selected_community()},
    handlerExpr = {
      annual_sensors() %...>%
        (function(s) {
          tryCatch(
            expr = {
              # Calculate the selected community location
              if ( selected_community() == 'all' ) {
                community_sensors <- s$meta
              } else {
                community_sensors <- s$meta[id2com(s$meta$communityRegion) == selected_community(),]
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
              # determine what sort of map update to do
              com <- unique(id2com(s$meta$communityRegion))
              if ( selected_community() != "all" ) {
                # Only show community group
                leaflet::leafletProxy("leaflet") %>%
                  leaflet::showGroup(group = com[com == selected_community()]) %>%
                  leaflet::hideGroup(group = com[com != selected_community()]) %>%
                  leaflet::removeMarker(layerId = "selected") # Remove the old marker selection
              } else {
                # Show all community groups
                leaflet::leafletProxy("leaflet") %>%
                  leaflet::showGroup(group = com)
              }
            },
            error = function(e) {
              logger.error(paste0("Error in community pick: ", e))
            }
          )
        })
    }
  )

  map_state <- reactiveValues('center' = NULL ,
                              'zoom' = NULL )
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {dates()},
    handlerExpr = {
      map_state$center <- input$leaflet_center
      map_state$zoom <- input$leaflet_zoom
      annual_sensors() %...>%
        (function(s) {
          tryCatch(
            expr = {
              leaflet::leafletProxy("leaflet", data = selected_marker()) %>%
                leaflet::removeMarker('selected') %>%
                leaflet::addCircleMarkers( lng = ~lng,
                                           lat = ~ lat,
                                           color = '#42434C',
                                           fillColor = '#EABA5E',
                                           fillOpacity = 1,
                                           radius = 9, opacity = 0.95,
                                           weight = 2, layerId  = 'selected') %>%
                leaflet::setView( lng = map_state$center[1],
                                  lat = map_state$center[2],
                                  zoom = map_state$zoom )
            },
            error = function(e) {
              logger.error(paste0("Error in community pick: ", e))
            }
          )
        })
    }
  )
}

