#' @include control.tune.r
roxygen()

#' @exportClass grid.control
#' @rdname grid.control 

setClass(
		"grid.control",
		contains = c("tune.control")
)

#' Control structure for grid search tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod grid.control
#' @rdname grid.control 
#' @title Control for grid search tuning. 


setGeneric(
		name = "grid.control",
		def = function(path) {
			if (missing(path))
				path=TRUE
			standardGeneric("grid.control")
		}
)


#' @rdname grid.control 

setMethod(
		f = "grid.control",
		signature = signature(path="logical"),
		def = function(path) {
			new("grid.control", path=path, start=list())
		}
)
