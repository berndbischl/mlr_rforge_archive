#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for neural network from package \code{nnet}.
#' @title NNet
#' @seealso \code{\link[nnet]{nnet}}
#' @export
setClass(
		"nnet.nn.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title NNet Constructor
setMethod(
		f = "initialize",
		signature = signature("nnet.nn.classif"),
		def = function(.Object, parset) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = TRUE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="NNet", learner.pack="nnet", learner.props=desc, parset=parset)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="nnet.nn.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			if (is.null(list(...)$size))
				nnet(f, data=.data, weights=.weights, size=1, ...)
			else 
				nnet(f, data=.data, weights=.weights, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "nnet.nn.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type = switch(.type, response="class", prob="raw")
			p = predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "class")
				p = as.factor(p)
			return(p)
		}
)	






