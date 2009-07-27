#' @export
setClass(
		"regr.props",
		contains = "learner.props"
)

setMethod(
		f = "as.character",
		signature = "regr.props",
		def = function(x) {
			return(
					paste(
							"Learner: \n",  
							"Supported features Nums:", x@supports.numerics, " Factors:", x@supports.factors, " Chars:", x@supports.characters, "\n",
							"Supports missings: ", x@supports.missing, "\n", 
							"Supports weights: ", x@supports.weights, "\n", 
							sep=""
					)
			)
		}
)




