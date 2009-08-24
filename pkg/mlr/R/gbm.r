#' @include wrapped.learner.regr.r 
roxygen()

#' @export
setClass(
		"gbm.regr", 
		contains = c("wrapped.learner.regr")
)


predict.gbm.regr <- function (object, newdata, type = "link", single.tree = FALSE, ...) {
	predict(object=object, newdata=newdata, n.trees=length(object$trees), type=type, single.tree=single.tree, ...)
}
	

#----------------- constructor ---------------------------------------------------------




setMethod(
		f = "initialize",
		signature = signature("gbm.regr"),
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "gbm"
			predict.fct <- "predict.gbm.regr"
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Gradient Boosting Machine", learner.pack="gbm",
					learner.model.class="gbm", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=list(distribution="gaussian", verbose=FALSE),
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
					learner.props=desc)
			return(.Object)
		}
)






