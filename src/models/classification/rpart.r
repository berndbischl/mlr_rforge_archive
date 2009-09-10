#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Classification Trees from package \code{rpart}.
#' 
#' \emph{Common hyperparameters:}
#' @title rpart.classif
#' @seealso \code{\link[rpart]{rpart}}
#' @export
setClass(
		"rpart.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title rpart Constructor

setMethod(
		f = "initialize",
		signature = signature("rpart.classif"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = TRUE
			)
			.Object <- callNextMethod(.Object, learner.name="RPART", learner.pack="rpart",
					train.fct="rpart", 
					learner.props=desc)
			return(.Object)
		}
)






