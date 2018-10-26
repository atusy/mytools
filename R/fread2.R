#' transposing fread
#'
#' @param input input data
#' @param transposing transpose data frame when rows indicate variables and cols indicate observations
#' @param remove_null TRUE in default to reduce data
#' @param ss name of spreadsheet software. Error values from the given ss are replaced by NA
#' @param args1 additional arguments given to first fread
#' @param args2 additional arguments given to second fread
#' @importFrom data.table fread
#' @importFrom stringr str_replace_all
#'
#' @export
fread2 <- function(input, transposing = TRUE, remove_null = TRUE, ss = c('excel', 'libre'), args1 = list(), args2 = list()) {

  if(!transposing) return(fread(input))

  #ABOUT ARGUMENTS#################################################
	#remove_null = TRUE removes null rows and columns
	#ss: spread sheet
	#args1: list of arguments to fread input data
	#args2: list of arguments to fread the transposed input data
	#When there are duplicated arguments in args1 and args2, the prior argument has priority
	#i.e., args1 = list(a = 1, a = 2, b = 3) is equivalent to args1 = list(a = 1, b = 3)

	#FOR TEST#######################################################
	#input = "ND0207.160129.csv"; remove_null = TRUE; ss = c('excel', 'libre'); args1 = list(); args2 = list()

	#read table as character matrix##################################
	#complete arguments
	args1 <- c(args1, input = input, header = FALSE, colClasses = 'character')
	#remove duplicated arguments
	args1 <- args1[!duplicated(names(args1))]
	#1st fread
	d <- as.matrix(do.call(data.table::fread, args1))
	rm(args1)

	#replace error messages to blank#####################################
	if(stringr::str_detect(tolower(ss[1]), '^(excel|libre)$')) {
		d <- matrix(stringr::str_replace_all(d, '^#.*\\!$', ''), nrow = nrow(d), ncol = ncol(d))
	}
	#replace \n to space
		d <- matrix(stringr::str_replace_all(d, '\n', ' '), nrow = nrow(d), ncol = ncol(d))

	#whether to remove rows and cols with no data############################
	if(remove_null){
		filled <- d != ''
		filled[is.na(filled)] <- FALSE
		rows <- rowSums(filled) != 0
		cols <- colSums(filled) != 0
	} else {
		rows <- rep(TRUE, nrow(d))
		cols <- rep(TRUE, ncol(d))
	}

	#transpose data########################################################
	args2 <- c(input = paste(apply(d[rows, cols], 2, paste, collapse = '\t'), collapse = '\n'), sep = '\t', args2, header = TRUE)
	args2 <- args2[!duplicated(names(args2))]
	do.call(data.table::fread, args2)
}

