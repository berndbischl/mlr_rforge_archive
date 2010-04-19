#' @include object.r
roxygen()
#' @include learner.props.r
roxygen()

#' Wraps an already implemented learning method from R to make it accesible to mlr.
#' 
#' Also includes a properties object to describe the features of the learner.     

#' @title Base class for inducers. 

setClass(
		"wrapped.learner",
		contains = c("object"),
		representation = representation(
				id = "character",
				label = "character",
				pack = "character",
				train.fct.pars = "list",
				predict.fct.pars = "list",
				props = "learner.props"
		)
)


#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title wrapped.learner constructor
setMethod(
		f = "initialize",
		signature = signature("wrapped.learner"),
		def = function(.Object, id, label, pack, props, parset=list()) {
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			
			if (missing(props))
				return(.Object)
						
			if (!missing(id))
				.Object@id = id
			else
				.Object@id = class(.Object) 
			.Object@label = label
			.Object@pack = pack
			
			.Object@train.fct.pars = parset
			.Object@predict.fct.pars = list()
			
			.Object@props = props
			return(.Object)
		}
)


#' Getter.
#' @rdname wrapped.learner-class

setMethod(
		f = "[",
		signature = signature("wrapped.learner"),
		def = function(x,i,j,...,drop) {
			callNextMethod()
		}
)



#' 
#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("wrapped.learner"),
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



#setGeneric(
#		name = "set.train.par",
#		def = function(learner, ...) {
#			standardGeneric("set.train.par")
#		}
#)
##' Set a parameter for the underlying train function of a 
##' [\code{\linkS4class{wrapped.learner}}].
##' This is not meant for hyperparameters, pass these through the usual parset argument, but rather to
##' fix (somewhat technical) arguments which stay the same for the whole experiment. You should not have to use this too often.
##' 
##' @param learner [\code{\linkS4class{wrapped.learner}}] \cr
##'   	The learner.
##' @param \ldots Parameters to fix in underlying train function. Have to be named.
##' 
##' @return \code{\linkS4class{wrapped.learner}} object with changed parameters for train function of the wrapped learner.
##'
##' @usage set.train.par(learner, \ldots)
##' 
##' @rdname set.train.par
##' @title Set parameter for training
#
#setMethod(
#		f = "set.train.par",
#		signature = signature("wrapped.learner"),
#		def = function(learner, ...) {
#			pars <- list(...)
#			pn <- names(pars)
#			learner@train.fct.pars[pn] <- pars
#			return(learner)
#		}
#)
#
#
#setGeneric(
#		name = "set.predict.par",
#		def = function(learner, ...) {
#			standardGeneric("set.predict.par")
#		}
#)
#
##' Set a parameter for the underlying predict function of a wrapped learner. 
##' Used to fix (somewhat techical) arguments which stay the same for the whole experiment.
##' You should not have to use this too often.
##'   
##' @param learner [\code{\linkS4class{wrapped.learner}}] \cr
##'   	Wrapping object for the underlying learner.
##' @param \ldots Parameters to fix in underlying predict function. Have to be named.
##' 
##' @return Wrapped.learner object with changed paramters for predict function.
##' 
##' @usage set.predict.par(learner, \ldots)
##'
##' @title Set parameter for prediction
##' @rdname set.predict.par
#
#setMethod(
#		f = "set.predict.par",
#		signature = signature("wrapped.learner"),
#		def = function(learner, ...) {
#			pars <- list(...)
#			pn <- names(pars)
#			learner@predict.fct.pars[pn] <- pars
#			return(learner)
#		}
#)



