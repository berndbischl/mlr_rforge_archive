

setClass(
		"tune.control",
		contains = c("opt.control"),
		representation = representation(
				lower = "list",
				upper = "list",
				ranges = "list",
				partypes = "character",
				scale = "function"
		)
)

setMethod(
		f = "initialize",
		signature = signature("tune.control"),
		def = function(.Object, minimize, tune.threshold, thresholds, lower, upper, ranges, partypes, scale) {
			if (missing(minimize))
				return(.Object)
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
