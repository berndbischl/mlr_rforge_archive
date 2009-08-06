
#' Since not all classifiers can deal with all kind of data properties of the data are compared 
#' with the possibilities of the classifier when a \code{\linkS4class{learn.task}} is generated. 
#' A classif.props object describes such classifier possibilities by logical values.
#' 
#' @slot supports.missing [logical] Can the classifier deal with missing values?
#' @slot supports.numerics [logical] Can the classifier deal with numeric variables?
#' @slot supports.factors [logical] Can the classifier deal with factor variables?
#' @slot supports.characters [logical] Can the classifier deal with character variables?
#' @slot supports.weights [logical] Can the classifier return class probabilities when predicting?
#' 
#'  @note  
#'  When a \code{\linkS4class{learn.task}} gets printed its classifier description is shown on the console.
#' 
#'  @exportClass learner.props

setClass(
		"learner.props",
		representation = representation(
				supports.missing = "logical",
				supports.numerics = "logical",
				#supports.integers = "logical",
				supports.factors = "logical",
				supports.characters = "logical",
				supports.weights = "logical"
		)
)

#' Displays the object
#' @param object The object
#' @export
setMethod(
		f = "show",
		signature = signature("learner.props"),
		def = function(object) {
			cat(as.character(object))
		}
)


#' Prints the object
#' @param object The object
#' @export
setMethod(
		f = "print",
		signature = signature("learner.props"),
		def = function(x, ...) {
			cat(as.character(x))
		}
)
