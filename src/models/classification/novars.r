#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"novars.classif", 
		contains = c("wrapped.learner.classif")
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
				.wrapped.learner="novars.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs,  ...) {
			list(targets=.data[, .targetvar])
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "novars.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			m <- .wrapped.model["learner.model"]
			tab <- prop.table(table(m$targets))
			probs <- as.numeric(tab) 
			
			if(.type=="response")
				sample(as.factor(names(tab)), nrow(.newdata), prob=probs, replace=TRUE)	
			else
				probs
		}
)	







