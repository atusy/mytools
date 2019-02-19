#' Convert qm_xmap objects to png
#'
#' @param x A matrix-like numeric object
#' @param nm A string to save `x` as png file.
#' @param .min,.max Minimum and maximum values for range of `scales::squish()`
#' @param lut Look up tables (viridis or gray). Partial matching is allowed.
#' @param ... Discarded.
#'
#' @importFrom pipeR %>>%
#' @importFrom grDevices
#'   col2rgb
#' @importFrom scales
#'   colour_ramp
#'   gradient_n_pal
#'   squish
#'   rescale
#'   viridis_pal
#' @importFrom png
#'   writePNG
#' @export
map2png <- function(x, nm = "map2png.png", .min = NA, .max = NA, lut = c("viridis", "gray"), ...) {
  UseMethod("map2png")
}

#' @rdname map2png
#' @export
map2png.default <- function(x, nm = "map2png", .min = NA, .max = NA, lut = c("viridis", "gray"), ...) {
  lut <- match.arg(lut)
  dims <- dim(x)
  x %>>%
    unlist(use.names = FALSE) %>>%
    scales::squish(range = c(.min, .max)) %>>%
    rescale() %>>%
    map2png_lut[[lut]]() %>>%
    col2rgb %>>%
    `/`(255L) %>>%
    t %>>%
    array(dim = c(dims, 3L)) %>>%
    writePNG(paste0(nm, ".png"))
  invisible(NULL)
}

#' @rdname map2png
#' @importFrom purrr walk2
#' @export
map2png.qm_xmap <- function(x, nm = "map2png", .min = NA, .max = NA, lut = "viridis", ...) {
  pwalk(
    list(x = x, nm = paste0(nm, names(x)), .min = .min, .max = .max, lut = lut),
    map2png.default
  )
}

map2png_lut <- list(
  gray = c("black", "white"),
  viridis = viridis_pal(1, 0, 1, 1, "D")(6)
) %>>%
  lapply(gradient_n_pal)
