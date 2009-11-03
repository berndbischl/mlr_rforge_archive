#' @include learner.props.r
roxygen()

#' Description object for the features of a learning algorithm.
#' 
#' @slot supports.multiclass Are multiclass problems ok?
#' @slot supports.probs Can probabilities be predicted?
#' @slot supports.costs Does the learner support cost-sensitive learning?
#' 
#' @exportClass classif.props

setClass(
		"classif.props",
		contains = c("learner.props"),
		representation = representation(
				supports.multiclass = "logical",
				supports.probs = "logical",
				supports.costs = "logical"
		)
)


#' Conversion to string.
setMethod(
		f = "as.character",
		signature = signature("classif.props"),
		def = function(x) {
			return(
					paste(
							"Learner: \n",  
							"Supports multiclass: ", x@supports.multiclass, "\n",
							"Supported features Nums:", x@supports.numerics, " Factors:", x@supports.factors, " Chars:", x@supports.characters, "\n",
							"Supports missings: ", x@supports.missing, "\n", 
							"Supports probabilities: ", x@supports.probs, "\n", 
							"Supports weights: ", x@supports.weights, "\n", 
							"Supports costs: ", x@supports.costs, "\n", 
							sep=""
					)
			)
		}
)


