#' @include object.r

setClass(
		"learner.desc",
		contains = c("object"),
		representation = representation(
				props = "list"
		)
)



#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("learner.desc"),
		def = function(.Object, missings, numerics, factors, characters, weights) {
			if (missing(missings))
				return(.Object)
			.Object@props$missings = missings 
			.Object@props$numerics = numerics 
			.Object@props$factors = factors 
			.Object@props$characters = characters 
			.Object@props$weights = weights
			return(.Object)
		}
)

#' @rdname learner.desc-class
setMethod(
		f = "[",
		signature = signature("learner.desc"),
		def = function(x,i,j,...,drop) {
			return(x@props[[i]])
		}
)



setClass(
		"learner.desc.regr",
		contains = c("learner.desc")
)


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("learner.desc.regr"),
		def = function(.Object, missings, numerics, factors, characters, weights) {
			callNextMethod(.Object, missings, numerics, factors, characters, weights)
		}
)



setClass(
		"learner.desc.classif",
		contains = c("learner.desc")
)


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("learner.desc.classif"),
		def = function(.Object, missings, numerics, factors, characters, weights, oneclass, twoclass, multiclass, probs, decision, costs) {
			.Object@props$multiclass = multiclass 
			.Object@props$probs = probs 
			.Object@props$decision = decision 
			.Object@props$costs = costs 
			callNextMethod(.Object, missings, numerics, factors, characters, weights)
		}
)

