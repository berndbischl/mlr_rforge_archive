#' @include learner.r
#' @include train.learner.r
#' @include predict.learner.r

setClass(
		"base.wrapper",
		contains = c("learner"),
		representation = representation(
			learner = "learner"
		)
)


#' Getter.
#' @rdname base.wrapper-class

setMethod(
		f = "[",
		signature = signature("base.wrapper"),
		def = function(x,i,j,...,drop) {
			if (i == "learner")
				return(x@learner)
			x@learner[i]
		}
)



setMethod(
		f = "initialize",
		signature = signature("base.wrapper"),
		def = function(.Object, learner) {
			if (missing(learner))
				return(.Object)
			.Object@learner = learner
			return(.Object)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="base.wrapper", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .weights, .costs,  ...) {
			train.learner(.learner@learner, .targetvar, .data, .weights, .costs,  ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "base.wrapper", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict.learner(.learner@learner, .model, .newdata, .type,  ...)
		}
)	



