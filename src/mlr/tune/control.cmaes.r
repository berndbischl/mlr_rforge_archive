#' @include control.tune.r
roxygen()

#' @exportClass cmaes.control
#' @rdname cmaes.control 

setClass(
		"cmaes.control",
		contains = c("tune.control")
)


#' Control structure for CMA-ES tuning. 
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
#' @exportMethod cmaes.control
#' @rdname cmaes.control 
#' @title Control for CMA-ES tuning. 


setGeneric(
		name = "cmaes.control",
		def = function(path, same.resampling.instance, start, ...) {
			if (missing(path))
				path = TRUE
      if (missing(same.resampling.instance))
        same.resampling.instance = TRUE
      if (missing(start))
				stop("You have to provide a start value!")
			standardGeneric("cmaes.control")
		}
)


#' @rdname cmaes.control 

setMethod(
		f = "cmaes.control",
		signature = signature(path="logical", same.resampling.instance="logical", start="numeric"),
		def = function(path, same.resampling.instance, start, ...) {
			new("cmaes.control", path=path, same.resampling.instance=same.resampling.instance, start=as.list(start), ...)
		}
)

