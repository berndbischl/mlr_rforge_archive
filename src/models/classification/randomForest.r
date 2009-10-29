#' @include wrapped.learner.classif.r 
roxygen()

#' Wrapped learner for Random Forests from package \code{randomForest} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{ntree}}{Number of trees to grow.}
#' 		\item{\code{mtry}}{Number of variables randomly sampled as candidates at each split.}
#' 		\item{\code{nodesize}}{Minimum size of terminal nodes.}
#' }
#' @title randomForest.classif
#' @seealso \code{\link[randomForest]{randomForest}}
#' @export
setClass(
		"randomForest.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Random Forest Constructor
setMethod(
		f = "initialize",
		signature = signature("randomForest.classif"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="randomForest", learner.pack="randomForest", learner.props=desc)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="randomForest.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			randomForest(f, data=.data, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "randomForest.classif", 
				.task = "classif.task", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .task, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="class", "response", "prob")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	









