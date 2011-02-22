#' @include control.tune.r
roxygen()

#' @exportClass grid.control
#' @rdname grid.control 

setClass(
		"grid.control",
		contains = c("TuneControl")
)

#' Control structure for grid search tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod grid.control
#' @rdname grid.control 
#' @title Control for grid search tuning. 


setGeneric(
		name = "grid.control",
		def = function(path, same.resampling.instance) {
			if (missing(path))
				path=TRUE
      if (missing(same.resampling.instance))
        same.resampling.instance = TRUE
      standardGeneric("grid.control")
		}
)


#' @rdname grid.control 

setMethod(
		f = "grid.control",
		signature = signature(path="logical", same.resampling.instance="logical"),
		def = function(path, same.resampling.instance) {
			new("grid.control", path=path, same.resampling.instance=same.resampling.instance, start=list())
		}
)
