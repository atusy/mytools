#' Normalize geochemical data and tydir::gather for spider diagram
#'
#' @param x Input data.frame. Components not in the std file won't be normalized.
#' @param keep Components which should not be normalized.
#' @param unit Chose unit of input data
#' @param std Standard file for normalization.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table fread
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom forcats fct_relevel
#' @importFrom pipeR %>>%
#' @importFrom purrr map2
#' @importFrom purrr pmap
#' @importFrom rlang UQ
#' @importFrom rlang set_names
#' @importFrom tidyr gather
#'
#' @export
norm_geochem <- function(
  x, keep = NULL, unit = 'ppm', std = '~/R/geochem/ms95.csv'
) {
  std <- fread(std)

  x_var <- names(x)
  elm <- if(is.null(keep)) {
    x_var[x_var %in% std$Element]
  } else {
    x_var[x_var %in% std$Element & !(x_var %in% keep)]
  }

  std_sel <-
    std %>>%
    filter(Element %in% rlang::UQ(elm)) %>>%
    select(rlang::UQ(unit), Element) %>>%
    set_names(c('x'), 'nm') %>>%
    pmap(set_names) %>>%
    unlist %>>%
    t %>>%
    as.data.table

  bind_cols(
    x %>>%
      select(-one_of(elm)),
    x %>>%
      select(one_of(elm)) %>>%
      map2(std_sel %>>% select(one_of(elm)), `/`)
  ) %>>%
    gather(var, val, -one_of(x_var[!(x_var %in% elm)])) %>>%
    mutate(var = forcats::fct_relevel(var, names(std_sel)))
}
