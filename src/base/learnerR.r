#' @include object.r
roxygen()
#' @include learner.props.r
roxygen()

#' Wraps an already implemented learning method from R to make it accesible to mlr.
#'  
#' Getter.\cr
#' 
#' \describe{
#'  \item{train.fct.pars [list]}{Paramters that will be passed to the underlying train function.}
#'  \item{predict.fct.pars [list]}{Paramters that will be passed to the underlying predict function.}
#' }
#' @title Base class for inducers. 

setClass(
		"rlearner",
		contains = c("learner"),
		representation = representation(
				id = "character",
				label = "character",
				pack = "character",
				props = "learner.props",
				train.fct.pars = "list",
				predict.fct.pars = "list",
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
				return(x@props@supports.probs)
			}
			if (i == "supports.decision") {
				return(x@props@supports.decision)
			}
			if (i == "pack") {
				return(x@pack)
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
		def = function(.Object, id, label, pack, props, parset=list()) {
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			
			if (missing(props))
				return(.Object)
			
			if (missing(id))
				id = class(.Object)
			if (missing(label))
				label = id
			.Object@id = id
			.Object@label = label
			.Object@pack = pack
			.Object@props = props
			.Object@train.fct.pars = parset
			.Object@predict.fct.pars = list()
			.Object@predict.type = "response"
			callNextMethod(.Object)
		}
)





#' 
#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("rlearner"),
		def = function(x) {
			ps = paste(names(x@train.fct.pars), x@train.fct.pars, sep="=", collapse=" ")
			return(paste(
							#todo regression. also check when applied to task!!
							"Classification learner ", x@label, " from package ", x@pack, "\n\n",					
							to.string(x@props), "\n",
							"Hyperparameters: ", ps, "\n",
							sep =""					
					))
		}
)
