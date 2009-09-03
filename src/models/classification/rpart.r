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
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "rpart"
			predict.fct <- "predict.rpart"
			par.for.classes = list(type="class")
			par.for.probs = list(type="class")
			trafo.for.classes = NULL
			trafo.for.probs = NULL
			
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
					learner.model.class="rpart", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=train.fct.pars, 
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars, 
					learner.props=desc)
			return(.Object)
		}
)






