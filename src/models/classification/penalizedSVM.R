#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


#' Wrapped learner for penalized Support Vector Machines from package \code{penalizedSVM} for classification problems.
#' 
#' \emph{General hyperparameters:}
#' \describe{
#' 		\item{\code{lambda1.set}}{set of tuning parameters lambda1}
#' 		\item{\code{fs.method}}{feature selection method. Availible 'scad' and '1norm'}
#' 		\item{\code{cross.outer}}{fold of outer cross validation, default is 0, no cv.}
#' 		\item{\code{nu}}{nu: weighted parameter}
#' 		\item{\code{calc.class.weights}}{calculate class.weights for SVM, default: FALSE}
#'   	\item{\code{output}}{0 - no output, 1 - produce output, default is 0}
#'   	\item{\code{seed}}{seed}
#' }

#' The kernel type and the hyperparameters are specified in \code{parset}. 
#' @title kernlab.svm.classif
#' @seealso \code{\link[penalizedSVM]{ksvm}}, \code{\link[penalizedSVM]{dots}}
#' @export
setClass(
		"svm.fs", 
		contains = c("wrapped.learner.classif")
)
#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title penalized SVM Constructor
setMethod(
		f = "initialize",
		signature = signature("svm.fs.classif"),
		def = function(.Object, parset) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE, 
					supports.decision = FALSE,
					supports.weights = TRUE,	
					supports.costs = FALSE 
			)
			
			callNextMethod(.Object, learner.name="svm.fs", learner.pack="penalizedSVM", learner.props=desc, parset=parset)
		}

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="svm.fs.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			kpar = list()
			args = list(...)
			args.names <- names(args)
		
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "svm.fs.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "response", "probabilities")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	
