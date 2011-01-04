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
#' @param start [numeric] \cr
#'   Named vector of initial values.
#' @param lower [numeric] \cr
#'   Named vector of lower boundary constraints. Default is -Inf. 
#' @param upper [numeric] \cr
#'	 Named vector of upper boundary constraints. Default is Inf. 
#' @param scale [\code{\link{function}}] \cr 
#'   A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'   Has to take a vector and return a scaled one. Default is identity function.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[cmaes]{cma_es}}.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod cmaes.control
#' @rdname cmaes.control 
#' @title Control for CMA-ES tuning. 


setGeneric(
		name = "cmaes.control",
		def = function(path, start, lower, upper, scale, ...) {
			if (missing(path))
				path = TRUE
			if (missing(start))
				stop("You have to provide a start value!")
      if(!all.names(start))
        stop("Argument start has to be properly named!")
			if (missing(lower))
			  lower=-Inf	
			if (missing(upper))
			  upper=Inf				
			if (missing(scale))
				scale=identity
			standardGeneric("cmaes.control")
		}
)


#' @rdname cmaes.control 

setMethod(
		f = "cmaes.control",
		signature = signature(path="logical", start="numeric", lower="numeric", upper="numeric", scale="function"),
		def = function(path, start, lower, upper, scale, ...) {
      pds = make.pds.from.lowup(names(start), lower, upper)
			new("cmaes.control", path=path,	start=as.list(start), par.descs=pds, scale=scale, ...)
		}
)

