#' @include wrapped.learner.regr.r
roxygen()


setClass(
		"gbm.regr", 
		contains = c("wrapped.learner.regr")
)


	

setMethod(
		f = "initialize",
		signature = signature("gbm.regr"),
		def = function(.Object, parset) {
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Gradient Boosting Machine", learner.pack="gbm", learner.props=desc, parset=parset)
			.Object <- set.train.par(.Object, distribution="gaussian", verbose=FALSE)
			.Object <- set.predict.par(.Object, type="link", single.tree = FALSE)
			return(.Object)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="gbm.regr", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			gbm(f, data=.data, weights=.weights, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "gbm.regr", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			m <- .wrapped.model["learner.model"]
			predict(m, newdata=.newdata, n.trees=length(m$trees), ...)
		}
)	







