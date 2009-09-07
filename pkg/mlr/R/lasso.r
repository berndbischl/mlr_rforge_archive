#' @include wrapped.learner.regr.r 
#' @include train.learner.r 
roxygen()

#' Wrapped learner for Lasso Regression from package \code{penalized} for regression problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{lambda1}}{Tuning parameter for L1 penalization.}			
#' 		\item{\code{steps}}{If greater than 1, the algorithm will fit the model for a range of steps lambda1-values, starting from the maximal value down to the value of lambda1 specified.}
#' 		\item{\code{epsilon}}{The convergence criterion.}
#' }
#' @title penalized.lasso
#' @seealso \code{\link[penalized]{penalized}}
#' @export
setClass(
		"penalized.lasso", 
		contains = c("wrapped.learner.regr")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Lasso Regression Constructor

setMethod(
		f = "initialize",
		signature = signature("penalized.lasso"),
		def = function(.Object, data, formula, train.fct.pars=list(), predict.fct.pars=list()) {
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Lasso regression", learner.pack="penalized",
					learner.model.class="penfit", learner.model.S4 = FALSE,
					train.fct="penalized", train.fct.pars=train.fct.pars,
					predict.fct="predict.penalized.lasso", predict.fct.pars=predict.fct.pars,
					learner.props=desc)
			return(.Object)
		}
)

#' @export
setMethod(
		f = "train.learner",
		
		signature = c(
				wrapped.learner="penalized.lasso", 
				formula="formula", 
				data="data.frame", 
				weights="numeric", 
				parset="list"
		),
		
		def = function(wrapped.learner, formula, data, weights, parset) {
			# allow lambda instead of lambda1
			ns <- names(parset)
			i <- which(ns == "lambda")
			if (length(i) > 0) 
				names(parset)[i] <- "lambda1" 
			m <- callNextMethod(wrapped.learner, formula, data, weights, parset)
			return(m)
		}
)


predict.penalized.lasso <- function(model, newdata) {
	predict(model, data=newdata, penalized=newdata[,names(model@penalized)])[,1]
}



