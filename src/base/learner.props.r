#' @include object.r

setClass(
		"learner.props",
		contains = c("object"),
		representation = representation(
				supports.missings = "logical",
				supports.numerics = "logical",
				#supports.integers = "logical",
				supports.factors = "logical",
				supports.characters = "logical",
				supports.weights = "logical"
		)
)

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

#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("classif.props"),
		def = function(x) {
			return(
					paste(
							sep=""
					)
			)
		}
)

setClass(
		"regr.props",
		contains = c("learner.props")
)


#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("regr.props"),
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


