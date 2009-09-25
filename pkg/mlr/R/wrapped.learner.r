#' @include learner.props.r
roxygen()

#' Wraps an already implemented learning method from R to make it accesible to mlr.
#' 
#' Also inlcudes a properties object to describe the features of the learner.     
#' @slot learner.name Descriptive name of the learning method
#' @slot learner.pack R package where learner is implemented
#' @slot train.fct Function used in above package to train a regular model in the package
#' @slot train.fct.pars Named list of parameters which are fixed in the above train.fct and used at every internal call.
#' @slot predict.fct Function used in above package to predict new data with a trained model 
#' @slot predict.newdata.arg Name of argument for the new data frame in the underlying predict method. 
#' @slot predict.fct.pars Named list of parameters which are fixed in the above predict.fct and used at every internal call.
#' @slot learner.props Properties of the learner 
#' @title wrapped.learner

setClass(
		"wrapped.learner",
		representation = representation(
				learner.name = "character",
				learner.pack = "character",
				train.fct = "function",
				train.fct.pars = "list",
				predict.fct = "function",
				predict.newdata.arg = "character",
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
			
			if(!require(learner.pack, character.only=TRUE)) {
				stop(paste("Learn.task for", learner.name, "could not be constructed! package", learner.pack, "missing!"))
			}
			
			if (is.character(train.fct)) {
				train.fct <- tryCatch(
						eval(substitute(getFromNamespace(x, learner.pack), list(x=train.fct))),
						error=function(e) eval(substitute(get(x), list(x=train.fct))))
			}
			if (is.character(predict.fct)) {
				predict.fct <- tryCatch(
						eval(substitute(getFromNamespace(x, learner.pack), list(x=predict.fct))),
						error=function(e) eval(substitute(get(x), list(x=predict.fct))))
			}

			.Object@learner.name <- learner.name
			.Object@learner.pack <- learner.pack
			
			.Object@train.fct <- train.fct
			.Object@train.fct.pars <- list()
			.Object@predict.fct <- predict.fct
			.Object@predict.newdata.arg <- predict.newdata.arg
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
			return(
					as.character(x@learner.props)					
			)
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

#' Set a parameter for the underlying train function of a wrapped learner. 
#' This is not meant for hyperparameters, pass these through the usual parset argument, but rather to
#' fix (somewhat technical) arguments which stay the same for the whole experiment. You should not have to use this too often.
#'   
#' @param object [\code{\linkS4class{learn.task}}] \cr
#'   	Learn task that contains the wrapped learner.
#' @param \ldots Parameters to fix in underlying train function. Have to be named.
#' 
#' @return learn.task object with changed parameters for train function of the wrapped learner.
#' 
#' @usage set.train.par(object, \ldots)
#'
#' @title set.train.par
#' @aliases set.train.par
#' @rdname set.train.par

setGeneric(
		name = "set.train.par",
		def = function(object, ...) {
			standardGeneric("set.train.par")
		}
)

#' @export
#' @rdname set.train.par
setMethod(
		f = "set.train.par",
		signature = signature("wrapped.learner"),
		def = function(object, ...) {
			pars <- list(...)
			pn <- names(pars)
			object@train.fct.pars[pn] <- pars
			return(object)
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

setGeneric(
		name = "set.predict.par",
		def = function(object, ...) {
			standardGeneric("set.predict.par")
		}
)

#' @export 
#' @rdname set.predict.par

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


