
# Show progress function
showLoad <-
  function(FUN) {

    shiny::withProgress(
      expr = FUN,
      value = 0.3,
      message = "Loading...",
      detail = "Please Wait."
    )

    return(FUN)

  }


# Error message handling
handleError <-
  function(expr, msg) {

    shiny::validate(
      shiny::need(expr, msg)
    )

  }

notify <-
  function(title = NULL, info = NULL, ...) {

    if ( is.null(title) ) title <- "Error"
    if ( is.null(info) ) info <- "Please select a different sensor."

    shiny::showNotification(
      type = "warning",
      ui = HTML(paste0("<b>", title, "</b> <br>", info)),
      ...
    )
  }

# sqamd color breaks
sqamd_break <-
  function(data) {
    return(cut(data, breaks = c(0,12, 35, 55, 75, 6000)))
  }


scale_fill_sqamd <- function(...) {

  sqamd_cols <-
    c(
      "purple2" = "#6b0096",
      "purple1" = "#9f00de",
      "blue3" = "#002ade",
      "blue2" = "#3b8aff",
      "blue1" = "#abebff"
    )

  ggplot2::scale_fill_manual(...,values = rev(stringr::str_to_upper(sqamd_cols)))

}

