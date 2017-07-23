#' transposing read_excel
#'
#' @param input input data
#' @param sheet numeric or character to select sheet
#' @param error_as_NA turn error as NA (e.g., #DIV/0!)
#' @param rm_blank_col boolean to decide removing blank columns
#'
#' @importFrom data.table fread
#' @importFrom readxl read_excel
#' @importFrom stringr str_replace_all
#'
#' @export
read_excel2 <- function(input, sheet = 1, transposing = TRUE, error_as_NA = TRUE, rm_blank_col = TRUE) {

  if(!transposing) return(read_excel(input, sheet = sheet))

  #read ods file as character matrix##################################
  d <- as.matrix(read_excel(input, sheet = sheet, col_names = FALSE, col_types = 'text'))

  #replace error messages to blank#####################################
  if(error_as_NA) {
    d <- matrix(stringr::str_replace_all(d, '^#.*\\!$', ''), nrow = nrow(d), ncol = ncol(d))
  }
  #replace \n to space
  d <- matrix(stringr::str_replace_all(d, '\n', ' '), nrow = nrow(d), ncol = ncol(d))

  #whether to remove rows and cols with no data############################
  if(rm_blank_col) {
    filled <- d != ''
    filled[is.na(filled)] <- FALSE
    rows <- rowSums(filled) != 0
    cols <- colSums(filled) != 0
  } else {
    rows <- rep(TRUE, nrow(d))
    cols <- rep(TRUE, ncol(d))
  }

  #transpose data########################################################
  fread(input = paste(apply(d[rows, cols], 2, paste, collapse = '\t'), collapse = '\n'), sep = '\t', header = TRUE)
}


