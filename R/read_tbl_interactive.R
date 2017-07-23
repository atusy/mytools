#' interactively read spreadsheet
#'
#' @importFrom readODS ods_sheets
#' @importFrom readxl excel_sheets
#' @importFrom stringr str_replace_all
#'
#' @param input leave NULL when interactive
#' @param sheet leave NULL when interactive
#' @param transposing leave NULL when interactive
#'
#' @export
read_tbl <- function(input = NULL, sheet = NULL, transposing = NULL) {
  if(is.null(input)) input <- file.choose()
  if(is.null(transpoding)) transposing <- menu(c('TRUE', 'FALSE'), title = 'Should input spreadsheet be transposed?')

  mime <- tolower(stringr::str_replace_all(input, '.*\\.', ''))

  reader <- if(grepl('^xlsx?$', mime)) {
      list(read_excel2, readxl::excel_sheets)
    } else if(grepl('^ods$', mime)) {
      list(read_ods2, readODS::ods_sheets)
    } else {
      return(fread2(input, transposing))
    }

  if(is.null(sheet)) sheet <- menu(reader[[2]](input), title = 'which sheet?')

  reader[[1]](input, sheet = sheet, transposing = transposing)

}
