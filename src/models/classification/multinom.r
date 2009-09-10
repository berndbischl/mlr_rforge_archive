#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Multinomial Regression from package \code{nnet} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' @title nnet.multinom
#' @seealso \code{\link[nnet]{multinom}}
#' @export
setClass(
		"nnet.multinom", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Multinomial Regression Constructor

setMethod(
		f = "initialize",
		signature = signature("nnet.multinom"),
		def = function(.Object) {

			#checked:
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name = "Multinomial regression", learner.pack = "nnet", 
					train.fct = "multinom", 
					predict.par.for.classes = list(),
					predict.par.for.probs = list(type="probs"),
					learner.props = desc)
			return(.Object)
		}
)