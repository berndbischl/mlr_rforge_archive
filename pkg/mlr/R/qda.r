#' @include wrapped.learner.classif.r
roxygen()

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
    def = function(.Object, data, formula, type="class") {

     desc = new("classif.props",
      supports.multiclass = TRUE,
      supports.missing = TRUE,
      supports.numerics = TRUE,
      supports.factors = TRUE,
      supports.characters = TRUE,
      supports.probs = TRUE,
	  supports.weights = FALSE
    )
      
    .Object <- callNextMethod(.Object, learner.name="qda", learner.pack="MASS",
      train.fct="qda", 
      learner.props=desc)
    return(.Object)
  }
)



