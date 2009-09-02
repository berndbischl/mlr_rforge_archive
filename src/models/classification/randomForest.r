#' @include wrapped.learner.classif.r 
roxygen()

#' Wrapped learner for Random Forests from package \code{randomForest} for classification problems.
#' @title randomForest.classif
#' @seealso \code{\link[randomForest]{randomForest}}
#' 
#' Hyperparameters:
#' \describe{
#' 		\item{\code{ntree}} {Number of trees to grow.}
#' 		\item{\code{mtry}} {Number of variables randomly sampled as candidates at each split.}
#' 		\item{\code{nodesize}} {Minimum size of terminal nodes.}
#' }
#' @export
setClass(
		"randomForest.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------

setMethod(
  f = "initialize",
  signature = signature("randomForest.classif"),
    def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
    train.fct <- "randomForest"
    predict.fct <- "predict.randomForest"
    
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
      learner.model.class="randomForest.formula", learner.model.S4 = FALSE,
      train.fct=train.fct, train.fct.pars=train.fct.pars, 
      predict.fct=predict.fct, predict.fct.pars=predict.fct.pars, 
	  predict.par.for.classes =list(type="response"),
	  predict.par.for.probs =list(type="prob"),
	  learner.props=desc)
    return(.Object)
  }
)









