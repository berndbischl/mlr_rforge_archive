
#' Description object for the features of a learning algorithm.
#' 
#' @slot supports.missing [logical] Can missing values be dealt with?
#' @slot supports.numerics [logical] Are numeric variables ok?
#' @slot supports.factors [logical] Are factor variables ok?
#' @slot supports.characters [logical] Are character variables ok?
#' @slot supports.weights [logical] Can case weights be dealt with?
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

#' Shows the object by calling as.character.
#' @param object The object
setMethod(
		f = "show",
		signature = signature("learner.props"),
		def = function(object) {
			cat(to.string(object))
		}
)


#' Prints the object by calling as.character.
#' @param object The object
setMethod(
		f = "print",
		signature = signature("learner.props"),
		def = function(x, ...) {
			cat(to.string(x))
		}
)
