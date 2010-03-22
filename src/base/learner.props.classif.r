#' @include learner.props.r
roxygen()


setClass(
		"classif.props",
		contains = c("learner.props"),
		representation = representation(
				supports.multiclass = "logical",
				supports.probs = "logical",
				supports.decision = "logical",
				supports.costs = "logical"
		)
)


setMethod(
		f = "to.string",
		signature = signature("classif.props"),
		def = function(x) {
			return(
					paste(
							"Supports multiclass: ", x@supports.multiclass, "\n",
							"Supported features Nums:", x@supports.numerics, " Factors:", x@supports.factors, " Chars:", x@supports.characters, "\n",
							"Supports missings: ", x@supports.missing, "\n", 
							"Supports probabilities: ", x@supports.probs, "\n", 
							"Supports decsion values: ", x@supports.decision, "\n", 
							"Supports weights: ", x@supports.weights, "\n", 
							"Supports costs: ", x@supports.costs, "\n", 
							sep=""
					)
			)
		}
)


