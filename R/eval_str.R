#' evaluate a string as a R code
#' @param x a string to evaluate
#' #' @export
eval_str <- function(x) eval(parse(text=as.character(x)))
