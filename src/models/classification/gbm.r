#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"classif.gbm", 
		contains = c("wrapped.learner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.gbm"),
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
			if (is.null(parset$distribution))
				parset$distribution = "adaboost"
			callNextMethod(.Object, label="Gradient Boosting Machine", pack="gbm", props=desc, parset=parset)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="classif.gbm", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			gbm(f, data=.data, weights=.weights, verbose=FALSE, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "classif.gbm", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			m <- .wrapped.model["learner.model"]
			predict(m, newdata=.newdata, type="link", n.trees=length(m$trees), single.tree=FALSE, ...)
		}
)	


