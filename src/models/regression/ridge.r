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
		def = function(.Object, data, target) {

			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, learner.name="ridge regression", learner.pack="penalized", learner.props=desc)
		}
)




#' Overwritten, to allow "lambda" instead of "lambda2" as parameter name.

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="penalized.ridge", 
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
				names(args)[i] = "lambda2"
			}
			pars <- list(f, data=.data)
			pars <- c(pars, args)
			do.call(penalized, pars)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "penalized.ridge", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, ...) {
			m <- .wrapped.model["learner.model"]
			.newdata[, .wrapped.model["target"]] <- 0
			predict(m, data=.newdata,  ...)[,"mu"]
		}
)	

