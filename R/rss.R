#' residual sum of squares
#' @param from matrix
#' @param to matrix
#' @export
rss <- function(from, to) apply(from, 1, function(x, y = t(to)) colSums((y - x) ^ 2))
