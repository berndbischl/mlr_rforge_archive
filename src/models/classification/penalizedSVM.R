#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()



setClass(
		"classif.lpsvm", 
		contains = c("wrapped.learner.classif")
)

setMethod(
		f = "initialize",
		signature = signature("classif.lpsvm"),
		def = function(.Object, parset) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE, 
					supports.decision = FALSE,
					supports.weights = TRUE,	
					supports.costs = FALSE 
			)
			
			callNextMethod(.Object, label="lpsvm", pack="penalizedSVM", props=desc, parset=parset)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="classif.lpsvm", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			kpar = list()
			args = list(...)
			args.names <- names(args)
		
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "classif.lpsvm", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "response", "probabilities")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	


