

setClass(
		"tune.control",
		contains = c("opt.control"),
		representation = representation(
				lower = "list",
				upper = "list",
				ranges = "list",
				partypes = "character",
				scale = "function",
				tune.threshold= "logical", 
				thresholds = "numeric"
		)
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
