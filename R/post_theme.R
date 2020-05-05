#' Theme for ggplot grapics
#'
#' @export
sf_theme <- function() {
  ggplot2::theme_bw(
    base_size = 16,
    base_family = "Palatino Linotype"
  )
}

#' Get main palettes colors
#'
#' @export
sf_colors <- function(...) {
  cols <- c(...)
  palette <- c(
    `red` = "#d11141",
    `green` = "#00b159",
    `blue` = "#00aedb",
    `orange` = "#f37735",
    `yellow` = "#ffc425",
    `light grey` = "#cccccc",
    `dark grey` = "#8c8c8c"
  )
  if (is.null(cols)) {
    return(palette)
  }
  return(palette[cols])
}

#' List of sf main palettes
#'
sf_palettes <- list(
  `main` = sf_colors("dark grey", "orange", "blue"),

  `cool` = sf_colors("blue", "green"),

  `hot` = sf_colors("yellow", "orange", "red"),

  `mixed` = sf_colors("blue", "green", "yellow", "orange", "red"),

  `grey` = sf_colors("light grey", "dark grey")
)

#' Return function to interpolate a sf color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
sf_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- sf_palettes[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  colorRampPalette(pal, ...)
}

#' Color scale constructor for sf colors
#'
#' @param palette Character name of palette in sf_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'   scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_color_sf <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- sf_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("sf_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for sf colors
#'
#' @param palette Character name of palette in sf_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
scale_fill_sf <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- sf_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("sf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
