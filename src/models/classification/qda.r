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

setMethod(
  f = "initialize",
  signature = signature("qda"),
    def = function(.Object, data, formula, type="class", train.fct.pars=list(), predict.fct.pars=list()) {
    train.fct <- "qda"
    predict.fct <- "predict.qda"

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
      learner.model.class="qda", learner.model.S4 = FALSE,
      train.fct=train.fct, train.fct.pars=train.fct.pars, 
      predict.fct=predict.fct, predict.fct.pars=predict.fct.pars, 
      learner.props=desc)
    return(.Object)
  }
)



