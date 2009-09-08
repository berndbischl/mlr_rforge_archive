#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Localized Linear Discriminant Analysis from package \code{MASS}.
#' @title loclda
#' @seealso \code{\link[klaR]{loclda}}
#' @export
setClass(
		"loclda", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title loclda Constructor

setMethod(
		f = "initialize",
		signature = signature("loclda"),
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "loclda" 
			predict.fct <- "predict" 
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.weights = FALSE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Localized LDA", learner.pack="klaR", 
					learner.model.class="loclda", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=train.fct.pars, 
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
					learner.props=desc)
			
			return(.Object)
		}
)





