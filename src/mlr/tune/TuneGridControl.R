#' @include TuneControl.R
roxygen()

#' Control structure for grid search tuning.
#' @exportClass TuneGridControl
#' @seealso \code{\link{makeTuneGridControl}}

setClass(
		"TuneGridControl",
		contains = c("TuneControl")
)

#' Create control structure for grid search tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod makeTuneGridControl
#' @rdname makeTuneGridControl 
#' @title Control for grid search tuning. 


setGeneric(
		name = "makeTuneGridControl",
		def = function(path, same.resampling.instance) {
			if (missing(path))
				path=TRUE
      if (missing(same.resampling.instance))
        same.resampling.instance = TRUE
      standardGeneric("makeTuneGridControl")
		}
)


#' @rdname makeTuneGridControl 

setMethod(
		f = "makeTuneGridControl",
		signature = signature(path="logical", same.resampling.instance="logical"),
		def = function(path, same.resampling.instance) {
			new("TuneGridControl", path=path, same.resampling.instance=same.resampling.instance, start=list())
		}
)
