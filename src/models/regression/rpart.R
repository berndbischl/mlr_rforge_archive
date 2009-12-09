#' @include wrapped.learner.regr.r
roxygen()

#' Wrapped learner for Regression Trees from package \code{rpart}.
#' 
#' \emph{Common hyperparameters:}
#' @title rpart.regr
#' @seealso \code{\link[rpart]{rpart}}
#' @export
setClass(
		"rpart.regr", 
		contains = c("wrapped.learner.regr")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title rpart Constructor
setMethod(
		f = "initialize",
		signature = signature("rpart.regr"),
		def = function(.Object) {
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			callNextMethod(.Object, learner.name="RPART", learner.pack="rpart",	learner.props=desc)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="rpart.regr", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			rpart(f, data=.data, weights=.weights, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "rpart.regr", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			predict(.wrapped.model["learner.model"], newdata=.newdata, ...)
		}
)	


