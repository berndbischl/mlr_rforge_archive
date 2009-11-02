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
		def = function(.Object, data, target) {
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, learner.name="Lasso regression", learner.pack="penalized", learner.props=desc)
		}
)


#' Overwritten, to allow "lambda" instead of "lambda1" as parameter name.

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="penalized.lasso", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, ...) {
			f = as.formula(paste(.targetvar, "~."))
			args = list(...)
			i = which(names(args) == "lambda") 
			if (length(i) > 0) {
				names(args)[i] = "lambda1"
			}
			pars <- list(f, data=.data)
			pars <- c(pars, args)
			do.call(penalized, pars)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "penalized.lasso", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, ...) {
			m <- .wrapped.model["learner.model"]
			.newdata[, wrapped.model["target"]] <- 0
			predict(m, data=.newdata,  ...)[,"mu"]
		}
)	




