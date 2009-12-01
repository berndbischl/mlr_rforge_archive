#' @include wrapped.learner.classif.r 


#' Wrapped learner for Naive Bayes from package \code{e1071} for classification problems.
#' @title naiveBayes
#' @seealso \code{\link[e1071]{naiveBayes}}
#' @export
setClass(
		"naiveBayes", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Naive Bayes Constructor
setMethod(
		f = "initialize",
		signature = signature("naiveBayes"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="Naive Bayes", learner.pack="e1071", learner.props=desc)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="naiveBayes", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			naiveBayes(f, data=.data, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "naiveBayes", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="class", "class", "raw")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	



