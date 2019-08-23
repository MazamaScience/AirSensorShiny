
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

# Colors for sqamd in ggplot fashion
scale_fill_sqamd <-
  function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

    # base colors - change these to change the apperance
    sqamd_colors <-
      c(
        "purple2" = "#810f7c",
        "purple1" = "#8c6bb1",
        "blue3" = "#8c96c6",
        "blue2" = "#9ebcda",
        "blue1" = "#bfd3e6"
      )

    sqamd_cols <-
      function(...) {
        cols <- c(...)
        if ( is.null(cols) ) return(sqamd_colors)
        sqamd_colors[cols]
      }

    sqamd_palettes <-
      list("main" = sqamd_cols())

    sqamd_pal <-
      function(palette = "main", reverse = FALSE, ...) {
        pal <- sqamd_palettes[[palette]]
        if (reverse) pal <- rev(pal)
        colorRampPalette(pal, ...)
      }

    pal <- sqamd_pal(palette = palette, reverse = reverse)

    if (discrete) {
      plot <-
        ggplot2::discrete_scale(
          "fill",
          paste0("sqamd_", palette),
          palette = pal,
          ...
        )
    } else {
      plot <-
        ggplot2::scale_fill_gradientn(colours = pal(256), ...)
    }

    return(plot)

  }

# sqamd color breaks
sqamd_break <-
  function(data) {
    return(cut(data, breaks = c(0,12, 35, 55, 75, 6000)))
  }
