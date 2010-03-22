#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Classification Trees from package \code{rpart}.
#' 
#' \emph{Common hyperparameters:}
#' @title rpart.classif
#' @seealso \code{\link[rpart]{rpart}}
#' @export
setClass(
		"rpart.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title rpart Constructor
setMethod(
		f = "initialize",
		signature = signature("rpart.classif"),
		def = function(.Object, ...) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = TRUE,
					supports.costs = TRUE
			)
			callNextMethod(.Object, learner.name="RPART", learner.pack="rpart",	learner.props=desc, ...)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="rpart.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			print("in rpart")
			print(list(...))
			rpart(f, data=.data, weights=.weights, parms=list(loss=.costs), ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "rpart.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type = switch(.type, prob="prob", "class")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	





