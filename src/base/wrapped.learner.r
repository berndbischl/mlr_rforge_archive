#' @include learner.props.r
roxygen()

#' @slot name Name of the classifier
#' @slot package R package where classifier is defined
#' @slot train.fct Function used in above package to train a regular model in the package (see \code{\link{train}})
#' @slot train.fct.pars Named list of parameters which are fixed in the above train.fct. NB: These are _NOT_ 
#' 		hyperparamters of the classifier but rather parameters which are fixed for the whole 
#' 		experiment and should be fixed at the beginning for convenience (Example would be the 
#'  	tolerance parameter in lda.)
#' @slot predict.fct Function used in above package to predict new data with a trained model 
#' 		(see \code{\link{predict}}) 
#' @slot predict.fct.pars Named list of parameters which are fixed in the above predict.fct. See train.fct.pars 
#' 		(example would be the method parameter in predict.lda.)
#' @slot predict.fct.trafo A function which, when applied to an output of the predict function, returns the vector of 
#' 		predicted class memberships the default function returns the output x if x is a factor, 
#' 		else returns x$class if possible or gives a warning.
#' @slot desc Contains logical values describing which kind of data the classifier can deal with e.g. whether it supports characters.
#' 		This is necessary to make sure that the classifier can deal with the given data 
#' 		(see \code{\linkS4class{classif.props}}).
#' @slot data Dataframe which includes all the data for the task.
#' @slot formula Specifies what output and inputs, e.g. Species ~ Petal.Length + Sepal.Length for iris.
#' @slot data.desc Contains logical values describing properties of the dataframe e.g. whether it has 
#' 		characters or missing values (see desc and \code{\linkS4class{data.desc}}).
#' @slot dummy.classes Does the predict function need a class column in the dataframe for prediction? 
#' 		If TRUE but no class column is avaible in the data a null column is generated 
#'	 	in predict (default is FALSE). 



setClass(
		"wrapped.learner",
		representation = representation(
				learner.name = "character",
				learner.pack = "character",
				train.fct = "function",
				train.fct.pars = "list",
				predict.fct = "function",
				predict.fct.pars = "list",
				learner.props = "learner.props"
		)
)


#---------------- constructor---- -----------------------------------------------------


setMethod(
		f = "initialize",
		signature = signature("wrapped.learner"),
		def = function(.Object, learner.name, learner.pack, learner.model.class, learner.model.S4,
				train.fct, train.fct.pars=list(), 
				predict.fct=predict, predict.fct.pars=list(), 
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
			.Object@predict.fct.pars <- predict.fct.pars
			
			
			.Object@learner.props <- learner.props
			return(.Object)
		}
)


setMethod(
		f = "as.character",
		signature = signature("wrapped.learner"),
		def = function(x) {
			return(
					as.character(x@learner.props)					
			)
		}
)

setMethod(
		f = "print",
		signature = signature("wrapped.learner"),
		def = function(x, ...) {
			cat(as.character(x))
		}
)


setMethod(
		f = "show",
		signature = signature("wrapped.learner"),
		def = function(object) {
			cat(as.character(object))
		}
)




