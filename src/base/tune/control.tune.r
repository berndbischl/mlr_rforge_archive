#' @include opt.control.r
roxygen()

#' Abstract base class for control objects for tuning. 
#' Cannot be instatiated. 
#' 
#' @exportClass tune.control
#' @seealso \code{\linkS4class{grid.control}} 
#' @title Base class for control objects for tuning.

setClass(
		"tune.control",
		contains = c("opt.control"),
		representation = representation(
				start = "list",
				lower = "list",
				upper = "list",
				ranges = "list",
				partypes = "character",
				scale = "function"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("tune.control"),
		def = function(.Object, minimize, tune.threshold, thresholds, start, lower, upper, ranges, partypes, scale) {
			if (missing(minimize))
				return(.Object)
			.Object@start = start 			
			.Object@lower = lower 			
			.Object@upper = upper 			
			.Object@ranges = ranges
			.Object@partypes = partypes 			
			.Object@scale = scale 		
			.Object = callNextMethod(.Object=.Object, minimize=minimize, 
					tune.threshold=tune.threshold, thresholds=thresholds)
			return(.Object)
		}
)


#' @rdname tune.control-class

setMethod(
		f = "[",
		signature = signature("tune.control"),
		def = function(x,i,j,...,drop) {
			if (i == "parnames") {
				if (!is.null(x@ranges))
					return(names(x@ranges))
				else
					return(names(x@lower))
			}
			callNextMethod(x,i,j,...,drop=drop)
		}
)
