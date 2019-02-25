#' A faster version of magick::image_ggplot
#' @inheritParams magick::image_ggplot
#' @inheritParams ggplot2::annotation_raster
#' @importFrom magick
#'   image_info
#' @importFrom ggplot2
#'   aes
#'   annotation_raster
#'   coord_fixed
#'   geom_blank
#'   ggplot
#'   scale_y_reverse
#'   theme_void
#' @export
image_ggplot2 <- function(image, interpolate = FALSE) {
  .info <- image_info(image)
  ggplot(data.frame(x = 0, y = 0), aes(x, y)) +
    geom_blank() +
    theme_void() +
    scale_y_reverse() +
    coord_fixed(expand = FALSE, xlim = c(0.5, .info$width + 0.5), ylim = c(0.5, .info$height + 0.5)) +
    annotation_raster(image, 0.5, .info$width + 0.5, - 0.5 - .info$height, - 0.5, interpolate = interpolate) +
    NULL
}
