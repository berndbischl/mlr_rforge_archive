#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


#' Wrapped learner for Support Vector Machines from package \code{kernlab} for classification problems.
#' 
#' \emph{General hyperparameters:}
#' \describe{
#' 		\item{\code{C}}{Cost of constraints violation (default: 1), this is the 'C'-constant of the regularization term in the Lagrange formulation.}
#' }
#' \emph{Kernel hyperparameters, sorted by kernel type:}
#' \describe{
#' 		\item{\code{rbfdot}}{\code{sigma}: inverse kernel width}
#' 		\item{\code{laplacedot}}{\code{sigma}: inverse kernel width}
#' 		\item{\code{polydot}}{\code{degree}: degree of the polynomial, \cr 
#' 								\code{scale}: scaling parameter of the polynomial, \cr 
#' 								\code{offset}: offset used in the polynomial}
#' 		\item{\code{tanhdot}}{\code{scale}: scaling parameter of the tangent kernel, \cr 
#' 								\code{offset}: offset used in the hyperbolic tangent kernel}
#' 		\item{\code{besseldot}}{\code{sigma}: inverse kernel width, \cr 
#' 								\code{ordner}: order of the Bessel function, \cr
#' 								\code{degree}: degree of the Bessel function}
#' 		\item{\code{anovadot}}{\code{sigma}: inverse kernel width, \cr 
#' 								\code{degree}: degree of the ANOVA kernel function}
#' 		\item{\code{stringdot}}{\code{length}: length of the strings considered, \cr
#' 								\code{lambda}: the decay factor, \cr 
#' 								\code{normalized}: logical parameter determining if the kernel evaluations should be normalized.} 
#' }
#' The kernel type and the hyperparameters are specified in \code{parset}. 
#' @title kernlab.svm.classif
#' @seealso \code{\link[kernlab]{ksvm}}, \code{\link[kernlab]{dots}}
#' @export
setClass(
		"kernlab.svm.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title SVM Constructor
setMethod(
		f = "initialize",
		signature = signature("kernlab.svm.classif"),
		def = function(.Object, parset) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.decision = TRUE,
					supports.weights = FALSE,	
					supports.costs = FALSE 
			)
			
			callNextMethod(.Object, learner.name="SVM", learner.pack="kernlab", learner.props=desc, parset=parset)
		}
)

#---------------- train -----------------------------------------------------


#' Overwritten, to allow direct passing of kernel hyperparameters.

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="kernlab.svm.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		# todo custom kernel. freezes? check mailing list
		# todo unify cla + regr, test all sigma stuff
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			pm = "prob" %in% .type 
			
			kpar = list()
			args = list(...)
			args.names <- names(args)
			
			kernel.par.names = c("degree", "offset", "scale", "sigma", "order", "length", "lambda")
#			kernel.par.names = c(kernel.par.names, paste("kpar", 0:9, sep=""))
			
			kpar = list()
			for (k in kernel.par.names) {
				x = args[[k]]
				if (!is.null(x)) {
					kpar[[k]] = x
					args[[k]] = NULL
				}
			}
			

			kargs = list(f, data=.data, prob.model = pm, fit=FALSE) 
			if (length(kpar) > 0)
				kargs$kpar = kpar
			
#			# there's a strange behaviour in r semantics here wgich forces this, see do.call and the comment about substitute
#			if (!is.null(args$kernel) && is.function(args$kernel) && !is(args$kernel,"kernel")) {
#				args$kernel = do.call(args$kernel, kpar)	
#			} 
			
			kargs = c(kargs, args)
			do.call(ksvm, kargs)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "kernlab.svm.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			.type <- switch(.type, prob="probabilities", decision="decision", "response")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	

