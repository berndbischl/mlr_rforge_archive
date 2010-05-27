#' @include object.r
roxygen()
#' @include learner.props.r
roxygen()

#' Wraps an already implemented learning method from R to make it accesible to mlr.
#'  
#' Getter.\cr
#' 
#' @title Base class for inducers. 

setClass(
		"rlearner",
		contains = c("learner"),
		representation = representation(
				id = "character",
				label = "character",
				pack = "character",
				props = "learner.props",
				predict.type = "character"
		)
)

#' Getter.
#' @rdname rlearner-class

setMethod(
		f = "[",
		signature = signature("rlearner"),
		def = function(x,i,j,...,drop) {
			if (i == "is.classif") {
				return(is(x, "rlearner.classif"))
			}
			if (i == "is.regr") {
				return(is(x, "rlearner.regr"))
			}
			if (i == "pack") {
				return(x@pack)
			}
			if (i == "supports.probs") {
				return(ifelse(x["is.regr"], F, x@props@supports.probs))
			}
			if (i == "supports.decision") {
				return(ifelse(x["is.regr"], F, x@props@supports.decision))
			}
			if (i == "supports.multiclass") {
				return(ifelse(x["is.regr"], F, x@props@supports.multiclass))
			}
			if (i == "supports.missings") {
				return(x@props@supports.missings)
			}
			if (i == "supports.costs") {
				return(ifelse(x["is.regr"], F, x@props@supports.costs))
			}
			if (i == "supports.weights") {
				return(x@props@supports.weights)
			}
			if (i == "supports.numerics") {
				return(x@props@supports.numerics)
			}
			if (i == "supports.factors") {
				return(x@props@supports.factors)
			}
			if (i == "supports.characters") {
				return(x@props@supports.characters)
			}
			if (i == "pack") {
				return(x@pack)
			}
			if (i == "predict.threshold") {
				return(x["hyper.pars"]$predict.threshold)
			}
			callNextMethod()
		}
)

#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title rlearner constructor
setMethod(
		f = "initialize",
		signature = signature("rlearner"),
		def = function(.Object, id, label, pack, props, parset.train=list(), parset.predict=list()) {
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			
			if (missing(props))
				return(.Object)
			if (missing(id))
				id = as.character(class(.Object))
			if (missing(label))
				label = id
			.Object@id = id
			.Object@label = label
			.Object@pack = pack
			.Object@props = props
			.Object@predict.type = "response"
			callNextMethod(.Object)
			.Object = set.hyper.pars(.Object, type="train", parset=parset.train)
			.Object = set.hyper.pars(.Object, type="predict", parset=parset.predict)
			return(.Object)
		}
)

#' @exportClass rlearner.classif
setClass("rlearner.classif", contains = c("rlearner"))

#' @exportClass rlearner.regr
setClass("rlearner.regr", contains = c("rlearner"))

