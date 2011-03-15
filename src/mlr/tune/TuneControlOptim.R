#' @include TuneControl.R
roxygen()

#' Control structure for tuning with \code{optim}.
#' @exportClass TuneControlOptim
#' @seealso \code{\link{makeTuneControlOptim}}

setClass(
		"TuneControlOptim",
		contains = c("TuneControl")
)


#' Create control structure for tuning with optim (Nelder-Mead, SANN, etc). 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param start [numeric] \cr
#'		Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[stats]{optim}}.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod makeTuneControlOptim
#' @rdname makeTuneControlOptim
#' @title Control for tuning with optim. 


setGeneric(
		name = "makeTuneControlOptim",
		def = function(path, same.resampling.instance, start, ...) {
			if (missing(path))
				path = TRUE
      if (missing(same.resampling.instance))
        same.resampling.instance = TRUE
      if (missing(start))
				stop("You have to provide a start value!")
			standardGeneric("makeTuneControlOptim")
		}
)


#' @rdname makeTuneControlOptim 

setMethod(
		f = "makeTuneControlOptim",
		signature = signature(path="logical", same.resampling.instance="logical", start="numeric"),
		def = function(path, same.resampling.instance, start, ...) {      
      new("TuneControlOptim", path=path, same.resampling.instance=same.resampling.instance, start=as.list(start), ...)
		}
)

