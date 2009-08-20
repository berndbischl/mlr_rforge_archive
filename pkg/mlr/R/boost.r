#' @include wrapped.learner.classif.r
roxygen()

#' @export
setClass(
		"adaboost", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------

setMethod(
  f = "initialize",
  signature = signature("adaboost"),
    def = function(.Object, data, formula, type="class", train.fct.pars=list(), predict.fct.pars=list()) {
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
      learner.model.class="boosting", learner.model.S4 = FALSE,
      train.fct=train.fct, train.fct.pars=train.fct.pars,
      predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
      learner.props=desc, dummy.classes=TRUE)
    return(.Object)
  }
)





