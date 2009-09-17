#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for Logistic Regression from package \code{stats} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{start}}{Starting values for the parameters in the linear predictor.}	
#' 		\item{\code{etastart}}{Starting values for the linear predictor.}
#' 		\item{\code{mustart}}{Starting values for the vector of means.}
#' }
#' @title logreg
#' @seealso \code{\link[stats]{glm}}
#' @export
setClass(
		"logreg", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Logistic Regression Constructor
#' @rdname logreg-class

setMethod(
		f = "initialize",
		signature = signature("logreg"),
		def = function(.Object) {
			
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
					train.fct="glm",  
					predict.par.for.classes = list(),
					predict.par.for.probs = list(type="response"),
					trafo.for.probs = function(x, wrapped.model) {
						m <- wrapped.model@learner.model												
						y <- matrix(0, ncol=2, nrow=length(x))
						resp <- model.response(model.frame(m$formula, m$data))
						levs <- levels(resp)
						colnames(y) <- levs
						y[,1] <- 1-x
						y[,2] <- x
						return(y)
					},
					trafo.for.classes = function(x, wrapped.model) {
						m <- wrapped.model@learner.model												
						y <- matrix(0, ncol=2, nrow=length(x))
						resp <- model.response(model.frame(m$formula, m$data))
						levs <- levels(resp)
						p <- as.factor(ifelse(x >= 0.5, levs[2], levs[1]))
						names(p) <- NULL
						return(p)
					},
					learner.props=desc)
			.Object <- set.train.par(.Object, maxit=100, family=binomial)
			return(.Object)
		}
)


