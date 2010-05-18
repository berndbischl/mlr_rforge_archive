#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()



setClass(
		"classif.lpsvm", 
		contains = c("rlearner.classif")
)

setMethod(
		f = "initialize",
		signature = signature("classif.lpsvm"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missings = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE, 
					supports.decision = FALSE,
					supports.weights = TRUE,	
					supports.costs = FALSE 
			)
			
			callNextMethod(.Object, label="lpsvm", pack="penalizedSVM", props=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.lpsvm", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		def = function(.learner, .targetvar, .data, .weights, .costs,  ...) {
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
				.learner = "classif.lpsvm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "response", "probabilities")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	


