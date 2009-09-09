#' @include learner.props.r
roxygen()

#' Wrapped.learner ---text!----
#' @slot learner.name Name of the learning method
#' @slot learner.pack R package where learner is defined
#' @slot train.fct Function used in above package to train a regular model in the package (see \code{\link{train}})
#' @slot train.fct.pars Named list of parameters which are fixed in the above train.fct. NB: These are _NOT_ 
#' 		hyperparamters of the classifier but rather parameters which are fixed for the whole 
#' 		experiment and should be fixed at the beginning for convenience (Example would be the 
#'  	tolerance parameter in lda.)
#' @slot predict.fct Function used in above package to predict new data with a trained model 
#' 		(see \code{\link{predict}}) 
#' @slot predict.newdata.arg Name of argument for the new data frame in the underlying predict method. 
#' @slot predict.fct.pars Named list of parameters which are fixed in the above predict.fct. See train.fct.pars 
#' 		(example would be the method parameter in predict.lda.)
#' @slot learner.props Properties of the learner 



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
		def = function(.Object, learner.name, learner.pack, learner.model.class, learner.model.S4,
				train.fct, train.fct.pars=list(), 
				predict.fct=predict, predict.newdata.arg="newdata", predict.fct.pars=list(), 
				learner.props) {
			
			
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
			
			#if (!learner.model.S4) {
			#  setOldClass(learner.model.class, where=.GlobalEnv)
			#}
			#setIs(learner.model.class, "mlr.external.model")      
			
			.Object@learner.name <- learner.name
			.Object@learner.pack <- learner.pack
			
			.Object@train.fct <- train.fct
			.Object@train.fct.pars <- train.fct.pars
			.Object@predict.fct <- predict.fct
			.Object@predict.newdata.arg <- predict.newdata.arg
			.Object@predict.fct.pars <- predict.fct.pars
			
			
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




