#' @include learner.props.r
#' @include wrapped.learner.r
roxygen()

#' Wrapped.learner.classif ---KORREKTURLESEN----
#' @slot train.par.for.classes  List of parameters for training (classification)
#' @slot train.par.for.probs  	List of parameters for training (regression)
#' @slot predict.par.for.classes	List of parameters for predicting (classification)
#' @slot predict.par.for.probs  	List of parameters for predicting (regression)
#' @slot trafo.for.classes  		Function, specifying a transformation for the classes
#' @slot trafo.for.probs  			Function, specifying a transformation for probabilities
#' @slot dummy.classes Does the predict function need a class column in the dataframe for prediction? 
#' 		If TRUE but no class column is avaible in the data a null column is generated 
#'   	 	in predict (default is FALSE). 
#' 
#' @title wrapped.learner.classif

setClass(
		"wrapped.learner.classif",
		contains = c("wrapped.learner"),
		representation = representation(
				train.par.for.classes = "list",
				train.par.for.probs = "list",
				predict.par.for.classes = "list",
				predict.par.for.probs = "list",
				trafo.for.classes = "function",
				trafo.for.probs   = "function",
				dummy.classes = "logical"
		)
)

#' Constructor.
#' @title wrapped.learner.classif constructor

setMethod(
		f = "initialize",
		signature = signature("wrapped.learner.classif"),
		def = function(
				.Object, 
				learner.name, 
				learner.pack, 
				train.fct, 
				predict.fct=predict,  
				learner.props,				
				train.par.for.classes = list(),
				train.par.for.probs = list(),
				predict.par.for.classes = list(type="class"),
				predict.par.for.probs = list(type="prob"),
				trafo.for.classes = "default",
				trafo.for.probs   = "default",
				dummy.classes = FALSE
		) {
			
			if (missing(learner.name))
				return(.Object)
			
			if (is.character(trafo.for.classes) && trafo.for.classes == "default") {
				trafo.for.classes <- function(x, model) {
					if (is.factor(x)) {
						return(x)
					} else if (is.list(x) && "class" %in% names(x)) {
						return(as.factor(x$class))
					}      
					logger.error("unkown return structure in predict fct! pls define predict.fct.trafo yourself!")
				}
			}
			if (is.character(trafo.for.probs) && trafo.for.probs == "default") {
				trafo.for.probs <- function(x, model) {
					if (is.matrix(x)) {
						return(x)
					} else if (is.list(x) && "posterior" %in% names(x)) {
						return(x$posterior)
					}      
					logger.error("unkown return structure in predict fct! pls define predict.fct.trafo yourself!")
				}
			}
			
			
			
			.Object@train.par.for.classes <- train.par.for.classes
			.Object@train.par.for.probs <- train.par.for.probs
			.Object@predict.par.for.classes <- predict.par.for.classes
			.Object@predict.par.for.probs <- predict.par.for.probs
			
			.Object@trafo.for.classes <- trafo.for.classes
			.Object@trafo.for.probs <- trafo.for.probs
			
			.Object@dummy.classes <- dummy.classes
			callNextMethod(.Object, 
					learner.name = learner.name, 
					learner.pack = learner.pack, 
					train.fct = train.fct,
					predict.fct = predict.fct,
					learner.props=learner.props
			)
		}
)