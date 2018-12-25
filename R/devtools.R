#' @importFrom here here
#' @noRd
.here <- quote(here::here())

#' @inherit devtools::load_all
#' @importFrom devtools load_all
#' @export
load_all <- devtools::load_all
formals(load_all)$path <- .here

#' @inherit devtools::test
#' @importFrom devtools test
#' @export
test <- devtools::test
formals(test)$path <- .here
