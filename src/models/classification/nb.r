#' @include wrapped.learner.classif.r 
roxygen()


#' @export
setClass(
		"classif.nb", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------

setMethod(
  f = "initialize",
  signature = signature("classif.nb"),
    def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
    train.fct <- "naiveBayes"
    predict.fct <- "predict.naiveBayes"

     desc = new("classif.props",
      supports.multiclass = TRUE,
      supports.missing = TRUE,
      supports.numerics = TRUE,
      supports.factors = TRUE,
      supports.characters = FALSE,
      supports.probs = TRUE,
	  supports.weights = FALSE
    )
      
      
    .Object <- callNextMethod(.Object, learner.name="nb", learner.pack="e1071",
      learner.model.class="naiveBayes", learner.model.S4 = FALSE,
      train.fct=train.fct, train.fct.pars=train.fct.pars,
      predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
	  predict.par.for.classes =list(type="class"),
	  predict.par.for.probs =list(type="raw"),
	  learner.props=desc)
    return(.Object)
  }
)






