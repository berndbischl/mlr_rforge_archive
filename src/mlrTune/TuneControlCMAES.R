#' @include TuneControl.R
roxygen()

#' Control structure for CMA-ES tuning. 
#' @exportClass TuneControlCMAES
#' @seealso \code{\link{makeTuneControlCMAES}}

setClass(
		"TuneControlCMAES",
		contains = c("TuneControl")
)


#' Create control structure for CMA-ES tuning. 
#' 
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param start [numeric] \cr
#'   Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[cmaes]{cma_es}}.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod makeTuneControlCMAES
#' @rdname makeTuneControlCMAES 
#' @title Control for CMA-ES tuning. 


setGeneric(
		name = "makeTuneControlCMAES",
		def = function(path, same.resampling.instance, start, ...) {
			if (missing(path))
				path = TRUE
      if (missing(same.resampling.instance))
        same.resampling.instance = TRUE
      if (missing(start))
				stop("You have to provide a start value!")
			standardGeneric("makeTuneControlCMAES")
		}
)


#' @rdname makeTuneControlCMAES 

setMethod(
		f = "makeTuneControlCMAES",
		signature = signature(path="logical", same.resampling.instance="logical", start="numeric"),
		def = function(path, same.resampling.instance, start, ...) {
			new("TuneControlCMAES", path=path, same.resampling.instance=same.resampling.instance, start=as.list(start), ...)
		}
)

