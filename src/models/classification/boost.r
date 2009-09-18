#' @include wrapped.learner.classif.r
roxygen()


#' Wrapped learner for Adaboost.M1 from package \code{adabag} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{minsplit}}{Minimum number of observations that must exist in a node in order for a split to be attempted.}			
#' 		\item{\code{cp}}{Complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.}
#' 		\item{\code{maxdepth}}{Maximum depth of any node of the final tree, with the root node counted as depth 0. Defaults to the number of classes.} 
#' }
#' @title adaboost
#' @seealso \code{\link[adabag]{adaboost.M1}}
#' @export
setClass(
		"adaboost", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------

#' Constructor.
#' @title Adaboost Constructor
setMethod(
  f = "initialize",
  signature = signature("adaboost"),
    def = function(.Object, data, formula, type="class") {
    train.fct <- "adaboost.M1"
    predict.fct <- "predict.boosting"

     desc = new("classif.props",
      supports.multiclass = TRUE,
      supports.missing = TRUE,
      supports.numerics = TRUE,
      supports.factors = TRUE,
      supports.characters = TRUE,
      supports.probs = FALSE,
	  supports.weights = FALSE
    )
      
      
    .Object <- callNextMethod(.Object, learner.name="boost", learner.pack="adabag",
      train.fct="adaboost.M1",
      learner.props=desc, dummy.classes=TRUE)
    return(.Object)
  }
)





