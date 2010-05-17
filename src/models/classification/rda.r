#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()

setClass(
		"classif.rda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.rda"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = FALSE,			
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, label="rda", pack="klaR", props=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.rda", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .weights, .costs, .type, ...) {
			f = as.formula(paste(.targetvar, "~."))
			rda(f, data=.data, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "classif.rda", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			p <- predict(.model["learner.model"], newdata=.newdata, ...)
			if (.type=="response")
				return(p$class)
			else
				return(p$posterior)
			
		}
)	





