# Kosten bei allen gecheckt.

#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"classif.ada", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.ada"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = TRUE,
					supports.costs = TRUE
			)
			
			.Object <- callNextMethod(.Object, label="Ada boosting", learner.pack="ada", props=desc)
			
			return(.Object)
		}
)

setMethod(
		f = "initialize",
		signature = signature("classif.ada"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, label="ada", pack="ada", props=desc)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.ada", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			if (!all(dim(.costs)) == 0) {
				lev = levels(.data[, .targetvar])
				.costs = .costs[lev, lev] 
				ada(f, data=.data, parms=list(loss=.costs), ...)
			} else
				ada(f, data=.data, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "classif.ada", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "vector", "prob")
			p = predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "prob")
				colnames(p) = .model["class.levels"] 
			return(p)
		}
)	



