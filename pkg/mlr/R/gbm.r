#' @include wrapped.learner.regr.r 
roxygen()

#' Wrapped learner for Gradient Boosting Machine from package \code{gbm} for regression problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{n.trees}}{Total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion.}			
#' 		\item{\code{interaction.depth}}{The maximum depth of variable interactions. 1 implies an additive model, 2 implies a model with up to 2-way interactions, etc.}
#' 		\item{\code{n.minobsinnode}}{Minimum number of observations in the trees terminal nodes.}
#' 		\item{\code{shrinkage}}{Shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction.}			
#' 		\item{\code{bag.fraction}}{Fraction of the training set observations randomly selected to propose the next tree in the expansion.}
#' }
#' @title gbm.regr
#' @seealso \code{\link[gbm]{gbm}}
#' @export
setClass(
		"gbm.regr", 
		contains = c("wrapped.learner.regr")
)


predict.gbm.regr <- function (object, newdata, ...) {
	predict(object=object, newdata=newdata, n.trees=length(object$trees), ...)
}
	

#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title GBM Constructor

setMethod(
		f = "initialize",
		signature = signature("gbm.regr"),
		def = function(.Object) {
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Gradient Boosting Machine", learner.pack="gbm",
					train.fct="gbm", predict.fct="predict.gbm.regr", 
					learner.props=desc)
			
			.Object <- set.train.par(.Object, distribution="gaussian", verbose=FALSE)
			.Object <- set.predict.par(.Object, type="link", single.tree = FALSE)
			return(.Object)
		}
)






