#' make report by rmarkdown
#' @param Rmd name of Rmd file
#' @param wd working directory
#' @param output_format all in default
#' @param browse whether or not to browse html file when it is in output
#'
#' @importFrom rmarkdown render
#' @importFrom stringr str_replace
#'
#' @export
report <- function(Rmd = 'report.Rmd', wd = NULL, output_format = 'all', browse = TRUE) {
  cd <- getwd()
  on.exit(setwd(cd))
  if(is.null(wd)) wd <- cd
  setwd(wd)
  render(Rmd, output_format)
  html <- str_replace(Rmd, '\\.[[:alnum:]]+', '.html')
  if(browse && file.exists(html)) browseURL(html)
}

