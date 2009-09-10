#' @include wrapped.learner.classif.r 
roxygen()

#' Wrapped learner for Random Forests from package \code{randomForest} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{ntree}}{Number of trees to grow.}
#' 		\item{\code{mtry}}{Number of variables randomly sampled as candidates at each split.}
#' 		\item{\code{nodesize}}{Minimum size of terminal nodes.}
#' }
#' @title randomForest.classif
#' @seealso \code{\link[randomForest]{randomForest}}
#' @export
setClass(
		"randomForest.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Random Forest Constructor

setMethod(
  f = "initialize",
  signature = signature("randomForest.classif"),
    def = function(.Object) {
    
     desc = new("classif.props",
      supports.multiclass = TRUE,
      supports.missing = TRUE,
      supports.numerics = TRUE,
      supports.factors = TRUE,
      supports.characters = TRUE,
      supports.probs = TRUE,
	  supports.weights = TRUE
    )
      
    .Object <- callNextMethod(.Object, learner.name="randomForest", learner.pack="randomForest",
      train.fct="randomForest", 
	  predict.par.for.classes =list(type="response"),
	  predict.par.for.probs =list(type="prob"),
	  learner.props=desc)
    return(.Object)
  }
)









