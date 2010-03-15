#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Multinomial Regression from package \code{nnet} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' @title nnet.multinom
#' @seealso \code{\link[nnet]{multinom}}
#' @export
setClass(
		"nnet.multinom", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Multinomial Regression Constructor
setMethod(
		f = "initialize",
		signature = signature("nnet.multinom"),
		def = function(.Object) {
			
			#checked:
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = TRUE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name = "Multinomial regression", learner.pack = "nnet", learner.props = desc)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="nnet.multinom", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			multinom(f, data=.data, weights=.weights, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "nnet.multinom", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "class", "probs")
			levs = .wrapped.model["class.levels"]
			p = predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "probs" && length(levs)==2) {
				p = matrix(c(1-p, p), ncol=2, byrow=FALSE)
				colnames(p) = levs
			} 
			return(p)
		}
)	







