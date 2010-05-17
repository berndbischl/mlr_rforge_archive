#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"novars.classif", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("novars.classif"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = TRUE
			)
			
			callNextMethod(.Object, label="NoVars", pack="mlr", props=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="novars.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .weights, .costs,  ...) {
			list(targets=.data[, .targetvar])
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "novars.classif", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			m <- .model["learner.model"]
			tab <- prop.table(table(m$targets))
			probs <- as.numeric(tab) 
			
			if(.type=="response")
				sample(as.factor(names(tab)), nrow(.newdata), prob=probs, replace=TRUE)	
			else
				probs
		}
)	







