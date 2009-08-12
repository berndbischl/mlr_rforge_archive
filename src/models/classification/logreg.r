#' @include wrapped.learner.classif.r
roxygen()

#'  \describe{	
#'  learn.task for classification using \code{\link[MASS]{lda}} from package MASS } 
#' 
#' \cr\cr\bold{Slots:}
#'  \describe{	
#'   \item{\code{learn.task[\linkS4class{learn.task}]}}{Specifies classifier and classification task }
#'  }
#' 
#' 
#'  @title t.lda
#' @export
setClass(
		"logreg", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------

setMethod(
		f = "initialize",
		signature = signature("logreg"),
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "glm" 
			predict.fct <- "predict" 
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name="logreg", learner.pack="stats", 
					learner.model.class="lm", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=train.fct.pars, 
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
					predict.par.for.classes = list(),
					predict.par.for.probs = list(type="prob"),
					learner.props=desc)
			return(.Object)
		}
)


