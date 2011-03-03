#' @include TuneControl.R
roxygen()

#' Control structure for CMA-ES tuning. 
#' @exportClass TuneCMAESControl
#' @seealso \code{\link{makeTuneCMAESControl}}

setClass(
		"TuneCMAESControl",
		contains = c("TuneControl")
)


#' Create control structure for CMA-ES tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param start [numeric] \cr
#'   Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[cmaes]{cma_es}}.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod makeTuneCMAESControl
#' @rdname makeTuneCMAESControl 
#' @title Control for CMA-ES tuning. 


setGeneric(
		name = "makeTuneCMAESControl",
		def = function(path, same.resampling.instance, start, ...) {
			if (missing(path))
				path = TRUE
      if (missing(same.resampling.instance))
        same.resampling.instance = TRUE
      if (missing(start))
				stop("You have to provide a start value!")
			standardGeneric("makeTuneCMAESControl")
		}
)


#' @rdname makeTuneCMAESControl 

setMethod(
		f = "makeTuneCMAESControl",
		signature = signature(path="logical", same.resampling.instance="logical", start="numeric"),
		def = function(path, same.resampling.instance, start, ...) {
			new("TuneCMAESControl", path=path, same.resampling.instance=same.resampling.instance, start=as.list(start), ...)
		}
)

