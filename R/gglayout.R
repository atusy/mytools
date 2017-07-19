#' evaluate a string as a R code
#' @param plots list of plots made by ggplot2
#' @param pos_row row positions of each plots
#' @param pos_col col positions of each plots
#'
#' @importFrom grid grid.newpage
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom grid grid.layout
#'
#' @export
gglayout = function(plots, pos_row = NULL, pos_col = NULL) {

	#automatically define a layout plots if pos_row and/or pos_col are NULL
	if(is.null(c(pos_row, pos_col))) {
		n <- ceiling(sqrt(length(plots)))
		pos_row <- rep_len(1:n, length(plots))
		pos_col <- unlist(lapply(1:n, rep, n))[1:length(plots)]
	} else if(is.null(pos_row) || is.null(pos_col)) {
		stop('define both pos_row and pos_col or leave both as NULL')
	}

	#layout plots
	grid::grid.newpage()
	grid::pushViewport(
		grid::viewport(
			layout = grid::grid.layout(
					max(unlist(pos_row)),
					max(unlist(pos_col))
				)
		)
	)

	#plot
	Map(
		function(plots, pos_row, pos_col) {
			print(
				plots,
				vp =  grid::viewport(layout.pos.row = pos_row, layout.pos.col = pos_col)
			)
		},
		plots,
		pos_row,
		pos_col
	)

	#returning NULL is required to avoid re-evaluating above plots
	invisible(NULL)
}

