#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"gbm.classif", 
		contains = c("wrapped.learner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("gbm.classif"),
		def = function(.Object, parset) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)			
			callNextMethod(.Object, learner.name="Gradient Boosting Machine", learner.pack="gbm", learner.props=desc, parset=parset)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="gbm.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			gbm(f, data=.data, weights=.weights, distribution="adabost", verbose=FALSE, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "gbm.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			m <- .wrapped.model["learner.model"]
			predict(m, newdata=.newdata, type="link", n.trees=length(m$trees), single.tree=FALSE, ...)
		}
)	


