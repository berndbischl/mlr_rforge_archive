#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"classif.gbm", 
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.gbm"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missings = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)			
			callNextMethod(.Object, label="GBM", pack="gbm", props=desc,
				parset.train=list(distribution = "adaboost"))

		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.gbm", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			gbm(f, data=.data, weights=.weights, verbose=FALSE, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "classif.gbm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			m <- .model["learner.model"]
			predict(m, newdata=.newdata, type="link", n.trees=length(m$trees), single.tree=FALSE, ...)
		}
)	


