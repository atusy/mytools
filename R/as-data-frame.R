#' @export
base::as.data.frame

#' @noRd
#' @export
as.data.frame.qm_xmap <- function() {
  out <- data.frame(
    expand.grid(
      row = seq(NROW(x[[1]])),
      col = seq(NCOL(x[[1]]))
    ),
    lapply(x, unlist, use.names = FALSE)
  )

  attributes(out)$qm_xmap <- attributes(x)

  out
}

formals(as.data.frame.qm_xmap) <- formals(as.data.frame)
