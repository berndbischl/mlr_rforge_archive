#' @include wrapped.learner.classif.r 
roxygen()

#' Wrapped learner for Regularized Discriminant Analysis from package \code{klaR} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{gamma}}		
#' 		\item{\code{lambda}}	
#' 		\item{\code{startsimplex}}{(Optional) a starting simplex for the Nelder-Mead-minimization.}	
#' }
#' @title rda
#' @seealso \code{\link[klaR]{rda}}
#' @export
setClass(
		"rda", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title RDA Constructor

setMethod(
		f = "initialize",
		signature = signature("rda"),
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "rda" 
			predict.fct <- "predict.rda"
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = FALSE,
					supports.weights = FALSE			
			)
			
			.Object <- callNextMethod(.Object, learner.name="rda", learner.pack="klaR", 
					learner.model.class="rda", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=train.fct.pars, 
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars, 
					learner.props=desc)
			return(.Object)
		}
)







