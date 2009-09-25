#' @include wrapped.learner.regr.r 
#' @include train.learner.r 
roxygen()

#' Wrapped learner for Ridge Regression from package \code{penalized} for regression problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{lambda2}}{Tuning parameter for L2 penalization.}			
#' 		\item{\code{epsilon}}{The convergence criterion.}
#' }
#' @title penalized.ridge
#' @seealso \code{\link[penalized]{penalized}}
#' @export
setClass(
		"penalized.ridge", 
		contains = c("wrapped.learner.regr")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Ridge Regression Constructor
setMethod(
		f = "initialize",
		signature = signature("penalized.ridge"),
		def = function(.Object, data, formula) {

			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			.Object <- callNextMethod(.Object, learner.name="ridge regression", learner.pack="penalized",
					train.fct="penalized", predict.fct="predict.penalized.ridge", 
					learner.props=desc)
			return(.Object)
		}
)



#' Overwritten, to allow "lambda" instead of "lambda2" as parameter name.
#' Besides that, simply delegates to super method.
#' 
#' @param wrapped.learner Object of class \code{\linkS4class{wrapped.learner}}.
#' @param formula A symbolic description of the model to be fitted.
#' @param data Dataframe which includes all the data for the task.
#' @param weights An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
#' @param parset Named list which contains the hyperparameters of the learner. Default is an empty list, which means no hyperparameters are specifically set and defaults of the underlying learner are used.
#' 
#' @export
setMethod(
		f = "train.learner",
		
		signature = c(
				wrapped.learner="penalized.ridge", 
				formula="formula", 
				data="data.frame", 
				weights="numeric", 
				parset="list"
		),
		
		def = function(wrapped.learner, formula, data, weights, parset) {
			# allow lambda instead of lambda2
			ns <- names(parset)
			i <- which(ns == "lambda")
			if (length(i) > 0) 
				names(parset)[i] <- "lambda2" 
			m <- callNextMethod(wrapped.learner, formula, data, weights, parset)
			return(m)
		}
)


predict.penalized.ridge <- function(model, newdata) {
	predict(model, data=newdata, penalized=newdata[,names(model@penalized)])[,1]
}



