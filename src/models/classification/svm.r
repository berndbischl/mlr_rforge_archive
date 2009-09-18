#' @include wrapped.learner.classif.r
#' @include train.learner.r
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
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.weights = FALSE	
			)
			
			.Object <- callNextMethod(.Object, learner.name="svm", learner.pack="kernlab",
					train.fct="ksvm", 
					train.par.for.classes =list(),
					train.par.for.probs =list(prob.model=TRUE),
					predict.par.for.classes =list(type="response"),
					predict.par.for.probs =list(type="probabilities"),
					learner.props=desc)
			return(.Object)
		}
)

#---------------- train -----------------------------------------------------

#' Overwritten, to allow direct passing of kernel hyperparameters.
#' Besides that, simply delegates to super method.
#' 
#' @param wrapped.learner Object of class \code{\linkS4class{wrapped.learner}}.
#' @param formula A symbolic description of the model to be fitted.
#' @param data Dataframe which includes all the data for the task.
#' @param weights An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
#' @param parset Named list which contains the hyperparameters of the learner. Default is an empty list, which means no hyperparameters are specifically set and defaults of the underlying learner are used.
#' 
#' @export
setMethod(
		f = "train.learner",
		
		signature = c(
				wrapped.learner="kernlab.svm.classif", 
				formula="formula", 
				data="data.frame", 
				weights="numeric", 
				parset="list"
		),
		
		def = function(wrapped.learner, formula, data, weights, parset) {
			
				k <- parset$kernel
				parset.names <- names(parset)
				kpar = list()
				
				make.kpar <- function(kernel.pars, kernel.name) {
					kpar <- list()
					for (p in kernel.pars) {
						if (p %in% parset.names)
							kpar[[p]] <- parset[[p]]
					}
					if (kernel.name %in% c("rbfdot", "laplacedot") && 
							(is.null(kpar$sigma) || kpar$sigma=="automatic")) {
						return("automatic")
					} else {
						return(kpar)
					}
				}
				
				change.parset <- function(parset, kpar) {
					for (p in names(kpar))
						parset[p] <- NULL
					parset$kpar = kpar
					return(parset)
				}
				
				if (is.null(k)) 
					k <- "rbfdot"      
				if (k == "rbfdot" || k == "laplacedot") 
					kpar <- make.kpar("sigma", k)
				if (k == "polydot") 
					kpar <- make.kpar(c("degree", "offset", "scale"), k)
				if (k == "tanhdot") 
					kpar <- make.kpar(c("offset", "scale"), k)
				if (k == "besseldot") 
					kpar <- make.kpar(c("degree", "sigma", "order"), k)
				if (k == "anovadot") 
					kpar <- make.kpar(c("degree", "sigma"), k)
				if (k == "anovadot") 
					kpar <- make.kpar(c("length", "lambda", "normalized"), k)
				
				parset <- change.parset(parset, kpar)
				
				m <- callNextMethod(wrapped.learner, formula, data, weights, parset)
				return(m)
			}
)
		





