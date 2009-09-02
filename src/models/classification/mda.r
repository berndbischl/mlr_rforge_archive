#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Mixture Discriminant Analysis from package \code{mda} for classification problems.
#' @title mda
#' @seealso \code{\link[mda]{mda}
#' 
#' Optional hyperparameters:
#' @export
setClass(
		"mda", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------

setMethod(
  f = "initialize",
  signature = signature("mda"),
    def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
    train.fct <- "mda"
    predict.fct <- "predict.mda"

	desc = new("classif.props",
			supports.multiclass = TRUE,
			supports.missing = FALSE,
			supports.numerics = TRUE,
			supports.factors = TRUE,
			supports.characters = FALSE,
			supports.probs = FALSE,
			supports.weights = FALSE
	)
		
    .Object <- callNextMethod(.Object, learner.name="mda", learner.pack="mda",
      learner.model.class="mda", learner.model.S4 = FALSE,
      train.fct=train.fct, train.fct.pars=train.fct.pars,
      predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
	  predict.par.for.classes =list(type="class"),
	  predict.par.for.probs =list(type="posterior"),
	  learner.props=desc)
#      data = data, formula=formula)
    return(.Object)
  }
)





