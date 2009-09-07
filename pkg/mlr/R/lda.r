#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Linear Discriminant Analysis from package \code{MASS}.
#' @title lda
#' @seealso \code{\link[MASS]{lda}}
#' @export
setClass(
		"lda", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title LDA Constructor

setMethod(
		f = "initialize",
		signature = signature("lda"),
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "lda" 
			predict.fct <- "predict.lda" 
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.weights = FALSE
			)
			
			.Object <- callNextMethod(.Object, learner.name="LDA", learner.pack="MASS", 
					learner.model.class="lda", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=train.fct.pars, 
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
					learner.props=desc)

			return(.Object)
		}
)





