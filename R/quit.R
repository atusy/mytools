#' short cut to terminate R
#' @export
# q <- function (save = "no", status = 0, runLast = TRUE) quit(save, status, runLast)
Q <- structure("no", class = "no")

#' @noRd
#' @export
print.no <- function(x, ...) {
  q(x)
  invisible(x)
}


