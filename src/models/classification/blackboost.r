#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


#' Wrapped learner for Classification Trees from package \code{rpart}.
#' 
#' \emph{Common hyperparameters:}
#' @title rpart.classif
#' @seealso \code{\link[rpart]{rpart}}
#' @export
setClass(
		"blackboost.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title rpart Constructor
setMethod(
		f = "initialize",
		signature = signature("blackboost.classif"),
		def = function(.Object, parset) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = TRUE,
					supports.costs = TRUE
			)
			callNextMethod(.Object, learner.name="blackboost", learner.pack="mboost", learner.props=desc, parset=parset)
		}
)


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="blackboost.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			blackboost(f, family=AdaExp(), data=.data, weights=.weights, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "blackboost.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "class", "link")
			#.wrapped.model["learner.model"]
			p = predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "prob") {
				y <- matrix(0, ncol=2, nrow=length(.newdata))
				colnames(y) <- .wrapped.model["class.levels"]
				y[,1] <- x
				y[,2] <- 1-x
				return(y)
			} else {
				return(p)
			}
		}
)	





