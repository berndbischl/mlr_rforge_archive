#' @include wrapped.learner.classif.r


#' Wrapped learner for Quadratic Discriminant Analysis from package \code{MASS} for classification problems.
#' @title qda
#' @seealso \code{\link[MASS]{qda}}
#' @export
setClass(
		"qda", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title QDA Constructor
setMethod(
		f = "initialize",
		signature = signature("qda"),
		def = function(.Object, data, target, type="class") {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.weights = FALSE,
					supports.costs = FALSE 
			)
			
			callNextMethod(.Object, learner.name="qda", learner.pack="MASS", learner.props=desc)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="qda", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			qda(f, data=.data, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "qda", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			p <- predict(.wrapped.model["learner.model"], newdata=.newdata, ...)
			if(.type=="class")
				return(p$class)
			else
				return(p$posterior)
		}
)	


