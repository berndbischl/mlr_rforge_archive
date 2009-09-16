#' @include wrapped.learner.classif.r 
roxygen()

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
	  supports.weights = FALSE
    )
      
      
    .Object <- callNextMethod(.Object, learner.name="Naive Bayes", learner.pack="e1071",
      train.fct="naiveBayes",
	  predict.par.for.classes =list(type="class"),
	  predict.par.for.probs =list(type="raw"),
	  learner.props=desc)
    return(.Object)
  }
)






