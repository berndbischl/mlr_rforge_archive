#' @include object.r

setClass(
		"learner.props",
		contains = c("object"),
		representation = representation(
				supports.missing = "logical",
				supports.numerics = "logical",
				#supports.integers = "logical",
				supports.factors = "logical",
				supports.characters = "logical",
				supports.weights = "logical"
		)
)

