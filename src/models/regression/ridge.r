#' @include wrapped.learner.regr.r 
#' @include train.learner.r 
roxygen()

#' Wrapped learner for Ridge Regression from package \code{penalized} for regression problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{lambda2}}{Tuning parameters for L2 penalization.}			
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

setMethod(
		f = "initialize",
		signature = signature("penalized.ridge"),
		def = function(.Object, data, formula, train.fct.pars=list(), predict.fct.pars=list()) {
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			.Object <- callNextMethod(.Object, learner.name="ridge regression", learner.pack="penalized",
					learner.model.class="penfit", learner.model.S4 = FALSE,
					train.fct="penalized", train.fct.pars=train.fct.pars,
					predict.fct="predict.penalized.ridge", predict.fct.pars=predict.fct.pars,
					learner.props=desc)
			return(.Object)
		}
)

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



