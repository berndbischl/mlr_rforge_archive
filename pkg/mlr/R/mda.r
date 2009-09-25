#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Mixture Discriminant Analysis from package \code{mda} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' @title mda
#' @seealso \code{\link[mda]{mda}}
#' @export
setClass(
		"mda", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title MDA Constructor
setMethod(
		f = "initialize",
		signature = signature("mda"),
		def = function(.Object) {
			
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
					train.fct="mda",
					predict.par.for.classes =list(type="class"),
					predict.par.for.probs =list(type="posterior"),
					learner.props=desc)
			return(.Object)
		}
)





