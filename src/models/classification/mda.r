#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"mda", 
		contains = c("wrapped.learner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("mda"),
		def = function(.Object, parset) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.decision = FALSE,
					supports.probs = TRUE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="mda", learner.pack="mda", learner.props=desc, parset=parset)
		}
)

#' @rdname train.learner


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="mda", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			mda(f, data=.data, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "mda", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "class", "posterior")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	




