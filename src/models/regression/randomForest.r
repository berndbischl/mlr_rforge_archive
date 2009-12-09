#' @include wrapped.learner.regr.r
roxygen()

#' Wrapped learner for Random Forests from package \code{randomForest} for regression problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{ntree}}{Number of trees to grow.}
#' 		\item{\code{mtry}}{Number of variables randomly sampled as candidates at each split.}
#' 		\item{\code{nodesize}}{Minimum size of terminal nodes.}
#' }
#' @title randomForest.regr
#' @seealso \code{\link[randomForest]{randomForest}}
#' @export
setClass(
		"randomForest.regr", 
		contains = c("wrapped.learner.regr")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Random Forest Constructor
setMethod(
		f = "initialize",
		signature = signature("randomForest.regr"),
		def = function(.Object) {
			
			desc = new("regr.props",
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, learner.name="randomForest", learner.pack="randomForest", learner.props=desc)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="randomForest.regr", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			randomForest(f, data=.data, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "randomForest.regr", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			predict(.wrapped.model["learner.model"], newdata=.newdata, ...)
		}
)	









