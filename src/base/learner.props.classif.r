#' @include learner.props.r
roxygen()

#' Since not all classifiers can deal with all kind of data properties of the data are compared 
#' with the possibilities of the classifier when a \code{\linkS4class{learn.task}} is generated. 
#' A classif.props object describes such classifier possibilities by logical values.
#'
#' @slot supports.multiclass bla
#' @slot supports.probs bla
#' 
#' @exportClass classif.props

setClass(
		"classif.props",
		contains = c("learner.props"),
		representation(
				supports.multiclass = "logical",
				supports.probs = "logical"
		)
)



#setMethod(
#		f = "as.character",
#		signature = signature("classif.props"),
#		def = function(x) {
#			return(
#					paste(
#							"Learner: \n",  
#							"Supports multiclass: ", x@supports.multiclass, "\n",
#							"Supported features Nums:", x@supports.numerics, " Factors:", x@supports.factors, " Chars:", x@supports.characters, "\n",
#							"Supports missings: ", x@supports.missing, "\n", 
#							"Supports probabilities: ", x@supports.probs, "\n", 
#							"Supports weights: ", x@supports.weights, "\n", 
#							sep=""
#					)
#			)
#		}
#)


