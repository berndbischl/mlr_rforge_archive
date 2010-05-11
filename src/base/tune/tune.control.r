

setClass(
		"tune.control",
		contains = c("object"),
		representation = representation(
				method = "character",
				lower = "list",
				upper = "list",
				ranges = "list",
				partypes = "character",
				minimize = "logical", 
				scale = "function",
				tune.threshold= "logical" 
		)
)


setMethod(
		f = "[",
		signature = signature("tune.control"),
		def = function(x,i,j,...,drop) {
			if (i == "parnames") {
				if (!is.null(ranges))
					return(names(x@ranges))
				else
					return(names(x@lower))
			}
			callNextMethod(x,i,j,...,drop=drop)
		}
)
