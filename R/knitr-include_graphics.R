#' Knitting docx?
#' @export
is_docx_output <- function() {
  pandocto <- knitr:::pandoc_to()
  !is.null(pandocto) && pandocto == 'docx'
}

#' Is x available and newer than y?
#' @param x,y filess to comapre
#' @export
is_newer <- function(x, y) {
  file.exists(x) && (file.mtime(x) > file.mtime(y))
}

#' A wrapper of knitr::include_graphics with file conversion
#' @param auto_pdf If knitting pdf and input path is svg file, use pdf file instead. Conversion is also performed if neccessary.
#' @param auto_png If knitting docx and input path is svg file, use png file instead. Conversion is also performed if neccessary.
#' @inheritParams knitr::include_graphics
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom rsvg rsvg_pdf
#' @importFrom rsvg rsvg_png
#' @export
include_graphics2 <- function (
  path, auto_pdf = TRUE, auto_png = TRUE, dpi = NULL
) {
  is_svg <- str_detect(path, '\\.svg$')
  png <- str_replace(path, 'svg$', 'png')
  pdf <- str_replace(path, 'svg$', 'pdf')
  if (is_svg && auto_png && is_docx_output()) {
    if(!is_newer(png, path)) rsvg_png(path, png)
    path <- png
  }
  if (is_svg && auto_pdf && knitr:::is_latex_output()) {
    if(!is_newer(pdf, path)) rsvg_pdf(path, pdf)
    path <- pdf
  }
  structure(path, class = c("knit_image_paths", "knit_asis"), dpi = dpi)
}

