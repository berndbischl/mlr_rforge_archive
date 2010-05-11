#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"classif.mda", 
		contains = c("wrapped.learner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.mda"),
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
			
			callNextMethod(.Object, label="mda", pack="mda", props=desc, parset=parset)
		}
)

#' @rdname train.learner


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="classif.mda", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			mda(f, data=.data, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "classif.mda", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "class", "posterior")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	




