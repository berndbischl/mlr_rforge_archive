#' @include wrapped.learner.classif.r
#' @include train.learner.r
roxygen()

#' Wrapped learner for boosting a binary response from package \code{ada}.
#' @title ada
#' @seealso \code{\link[ada]{ada}}
#' @export
setClass(
		"ada", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Ada constructor
setMethod(
		f = "initialize",
		signature = signature("ada"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = TRUE,
					supports.costs = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Ada boosting", learner.pack="ada", 
					train.fct="ada",  
					learner.props=desc)
			
			return(.Object)
		}
)

#' Overwritten, to allow direct passing of tree hyperparameters in rpart.control.
#' Besides that, simply delegates to super method.
#' @export
setMethod(
		f = "train.learner",
		
		signature = c(
				.wrapped.learner="ada", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.parset="list"
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .parset) {
			
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
			
			m <- callNextMethod(wrapped.learner, target, data, weights, parset)
			return(m)
		}
)

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
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			kpar = list()
			args = list(...)
			args.names <- names(args)
			
			make.kpar <- function(kernel.pars, kernel.name) {
				kpar <- list()
				for (p in kernel.pars) {
					if (p %in% args.names)
						kpar[[p]] <- args[[p]]
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
			
			if (!("kernel" %in% args.names)) 
				kernel <- "rbfdot" 
			else
				kernel <- args$kernel
			
			if (kernel == "rbfdot" || kernel == "laplacedot") 
				kpar <- make.kpar("sigma", kernel)
			if (kernel == "polydot") 
				kpar <- make.kpar(c("degree", "offset", "scale"), kernel)
			if (kernel == "tanhdot") 
				kpar <- make.kpar(c("offset", "scale"), kernel)
			if (kernel == "besseldot") 
				kpar <- make.kpar(c("degree", "sigma", "order"), kernel)
			if (kernel == "anovadot") 
				kpar <- make.kpar(c("degree", "sigma"), kernel)
			if (kernel == "anovadot") 
				kpar <- make.kpar(c("length", "lambda", "normalized"), kernel)
			
			parset = list(f, data=.data, prob.model = (.type == "prob"), fit=FALSE)
			parset = c(parset, args)
			parset <- change.parset(parset, kpar)
			
			do.call(ksvm, parset)
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
			.type <- ifelse(.type=="class", "response", "probabilities")
			predict(.wrapped.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	



