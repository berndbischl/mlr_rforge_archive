

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

setMethod(
		f = "show",
		signature = signature("learner.props"),
		def = function(object) {
			cat(to.string(object))
		}
)


setMethod(
		f = "print",
		signature = signature("learner.props"),
		def = function(x, ...) {
			cat(to.string(x))
		}
)
