#' @include wrapped.learner.classif.r
roxygen()


#' Wrapped learner for boosting a binary response from package \code{ada}.
#' @title ada
#' @seealso \code{\link[ada]{ada}}
#' @export
setClass(
		"ada", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Ada constructor
setMethod(
		f = "initialize",
		signature = signature("ada"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = TRUE,
					supports.costs = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Ada boosting", learner.pack="ada", 
					train.fct="ada",  
					learner.props=desc)
			
			return(.Object)
		}
)

setMethod(
		f = "initialize",
		signature = signature("ada"),
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
			
			callNextMethod(.Object, learner.name="ada", learner.pack="ada", learner.props=desc)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="ada", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			ada(f, data=.data, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "ada", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="class", "vector", "prob")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	



