
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

