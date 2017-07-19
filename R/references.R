#' list up references in manuscript
#' @param x a file name
#' @importFrom readr read_lines
#' @importFrom pipeR %>>%
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @export
references <- function(x) {
  #x <- '/home/atusy/Univ/!2017D3/ND0207/ND0207.Rmd'
  x %>>%
  read_lines %>>%
  paste(collapse = '') %>>%
  str_extract_all('[:alpha:]+(( (and|AND|And|&) [:alpha:]+)|( et al\\.))?(, | \\()[12][0-9]{3}') %>>%
  unlist(use.names = FALSE, recursive = FALSE) %>>%
  str_replace_all(',', '') %>>%
  str_replace_all('\\(', '') %>>%
  unique %>>%
  sort %>>%
  paste(collapse = '\n') %>>%
  cat
}
  #Hoge, 1992
  #Hoge & Hoge, 1992
  #Hoge et al., 1992

  #Hoge (1992)
  #Hoge & Hoge (1992)
  #Hoge et al. (1992)

