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
setClass("lda", contains="wrapped.learner.classif")


#----------------- constructor ---------------------------------------------------------

setMethod(
		f = "initialize",
		signature = "lda",
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





