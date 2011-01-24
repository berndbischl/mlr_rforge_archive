#' @include control.tune.r
roxygen()

#' @exportClass optim.control
#' @rdname optim.control 

setClass(
		"optim.control",
		contains = c("tune.control")
)


#' Control structure for tuning with optim (Nelder-Mead, SANN, etc). 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param start [numeric] \cr
#'		Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[stats]{optim}}.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod optim.control
#' @rdname optim.control 
#' @title Control for tuning with optim. 


setGeneric(
		name = "optim.control",
		def = function(path, start, ...) {
			if (missing(path))
				path = TRUE
			if (missing(start))
				stop("You have to provide a start value!")
      if(!all.els.named(start))
        stop("Argument start has to be properly named!")
			standardGeneric("optim.control")
		}
)


#' @rdname optim.control 

setMethod(
		f = "optim.control",
		signature = signature(path="logical", start="numeric"),
		def = function(path, start, ...) {
      pds = make.pds.from.lowup(names(start))
      new("optim.control", path=path, start=as.list(start), par.descs=pds, ...)
		}
)

