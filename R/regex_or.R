#' produce regular expression of "or" (e.g., ^(abc)|(def)$)
#' @param x a character vector
#' @export
regex_or <- function(x) paste0('^(', paste(x, collapse=')|('), ')$')

