#' @include wrapped.learner.classif.r
#' @include wrapped.model.r
#' @include train.learner.r
roxygen()

#' @export
setClass("kernlab.svm.classif", contains="wrapped.learner.classif")


#----------------- constructor ---------------------------------------------------------

setMethod(
		f = "initialize",
		signature = "kernlab.svm.classif",
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "ksvm"
			predict.fct <- "predict"
			
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
					learner.model.class="ksvm", learner.model.S4 = TRUE,
					train.fct=train.fct, train.fct.pars=train.fct.pars,
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
					train.par.for.classes =list(),
					train.par.for.probs =list(prob.model=TRUE),
					predict.par.for.classes =list(type="response"),
					predict.par.for.probs =list(type="probabilities"),
					learner.props=desc)
			return(.Object)
		}
)

#---------------- train -----------------------------------------------------

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
		





