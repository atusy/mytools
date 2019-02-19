#' binning for matrix-like object
#' @param x A matrix-like object
#' @param size Binning size (default: `2L`)
#' @param ... Discarded
#' @importFrom dplyr
#'   group_by
#'   mutate
#'   summarize
#'   ungroup
#'   arrange
#'   pull
#' @export
binning <- function(x, size = 2L, ...) {
  UseMethod("binning")
}

#' @export
binning.default <- function(x, size = 2L, ...) {
  asis <- list(
    data.frame = as.data.frame,
    matrix = identity
  )[[class(x)]]
  expand.grid(
    rows = seq(0L, NROW(x) - 1L),
    cols = seq(0L, NCOL(x) - 1L)
  ) %>>%
    `%/%`(size) %>>%
    mutate(x = c(unlist(x, use.names = FALSE))) %>>%
    group_by(rows, cols) %>>%
    summarize(x = sum(x)) %>>%
    ungroup %>>%
    arrange(cols, rows) %>>%
    pull(x) %>>%
    matrix(nrow = nrow(x) / size) %>>%
    asis
}

#' @export
binning.qm_xmap <- function(x, size = 2L, ...) {
  structure(
    lapply(x, binning.default, size = size),
    class = class(x)
  )
}
