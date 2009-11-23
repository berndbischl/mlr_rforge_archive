#' @include learner.props.r
roxygen()

#' Wraps an already implemented learning method from R to make it accesible to mlr.
#' 
#' Also inlcudes a properties object to describe the features of the learner.     
#' @slot learner.name Descriptive name of the learning method
#' @slot learner.pack R package where learner is implemented
#' @slot train.fct.pars Named list of parameters which are fixed in an internal call to the underlying train function of the learner.
#' @slot predict.fct.pars Named list of parameters which are fixed in an internal call to the underlying predict function of the learner.
#' @slot learner.props Properties of the learner 
#' @title wrapped.learner

setClass(
		"wrapped.learner",
		representation = representation(
				learner.name = "character",
				learner.pack = "character",
				train.fct.pars = "list",
				predict.fct.pars = "list",
				learner.props = "learner.props"
		)
)


#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title wrapped.learner constructor
setMethod(
		f = "initialize",
		signature = signature("wrapped.learner"),
		def = function(.Object, learner.name, learner.pack, train.fct, predict.fct=predict, predict.newdata.arg="newdata", learner.props) {
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			
			if (missing(learner.name))
				return(.Object)
			
			if(learner.pack != "mlr" && !require(learner.pack, character.only=TRUE)) {
				stop(paste("Learn.task for", learner.name, "could not be constructed! package", learner.pack, "missing!"))
			}

			.Object@learner.name <- learner.name
			.Object@learner.pack <- learner.pack
			
			.Object@train.fct.pars <- list()
			.Object@predict.fct.pars <- list()
			
			.Object@learner.props <- learner.props
			return(.Object)
		}
)

#' Conversion to string.
setMethod(
		f = "as.character",
		signature = signature("wrapped.learner"),
		def = function(x) {
			return(paste( 
					"Classification learner ", x@learner.name, " from package ", x@learner.pack, "\n\n",					
					as.character(x@learner.props), 
					sep =""					
			))
		}
)

#' Prints the object by calling as.character.
setMethod(
		f = "print",
		signature = signature("wrapped.learner"),
		def = function(x, ...) {
			cat(as.character(x))
		}
)

#' Shows the object by calling as.character.
setMethod(
		f = "show",
		signature = signature("wrapped.learner"),
		def = function(object) {
			cat(as.character(object))
		}
)


setGeneric(
		name = "set.train.par",
		def = function(learner, ...) {
			standardGeneric("set.train.par")
		}
)
#' Set a parameter for the underlying train function of a 
#' [\code{\linkS4class{wrapped.learner}}].
#' This is not meant for hyperparameters, pass these through the usual parset argument, but rather to
#' fix (somewhat technical) arguments which stay the same for the whole experiment. You should not have to use this too often.
#' 
#' @param object [\code{\linkS4class{wrapped.learner}}] \cr
#'   	The learner.
#' @param \ldots Parameters to fix in underlying train function. Have to be named.
#' 
#' @return \code{\linkS4class{wrapped.learner}} object with changed parameters for train function of the wrapped learner.
#'
#' @rdname set.train.par
#' @export 

setMethod(
		f = "set.train.par",
		signature = signature("wrapped.learner"),
		def = function(learner, ...) {
			pars <- list(...)
			pn <- names(pars)
			object@train.fct.pars[pn] <- pars
			return(object)
		}
)


#' Set a parameter for the underlying predict function of a 
#' [\code{\linkS4class{wrapped.learner}}] or - for convienience - a [\code{\linkS4class{learn.task}}].
#' Used to fix (somewhat techical) arguments which stay the same for the whole experiment.
#' You should not have to use this too often.
#' 
#' @usage set.predict.par(object, \ldots)
#' @title set.predict.par
#' @rdname set.predict.par
#' @export 


setGeneric(
		name = "set.predict.par",
		def = function(object, ...) {
			standardGeneric("set.predict.par")
		}
)

#' Set a parameter for the underlying predict function of a wrapped learner. 
#' Used to fix (somewhat techical) arguments which stay the same for the whole experiment.
#' You should not have to use this too often.
#'   
#' @param object [\code{\linkS4class{wrapped.learner}}] \cr
#'   	Wrapping object for the underlying learner.
#' @param \ldots Parameters to fix in underlying predict function. Have to be named.
#' 
#' @return Wrapped.learner object with changed paramters for predict function.
#' 
#' @usage set.predict.par(object, \ldots)
#'
#' @title set.predict.par
#' @rdname set.predict.par
#' @export 

setMethod(
		f = "set.predict.par",
		signature = signature("wrapped.learner"),
		def = function(object, ...) {
			pars <- list(...)
			pn <- names(pars)
			object@predict.fct.pars[pn] <- pars
			return(object)
		}
)


