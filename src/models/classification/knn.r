#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for k-Nearest Neighbor from package \code{kknn} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{k}}{Number of neighbors considered.} 	
#' 		\item{\code{distance}}{Parameter of Minkowski distance.}
#' }
#' @title kknn.classif
#' @seealso \code{\link[kknn]{kknn}}
#' @export
setClass(
		"kknn.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title kNN (classification) Constructor
setMethod(
		f = "initialize",
		signature = signature("kknn.classif"),
		def = function(.Object) {
			
			desc <- new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="knn", learner.pack="kknn", learner.props=desc)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="kknn.classif", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			list(target=.targetvar, data=.data, parset=list(...))
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "kknn.classif", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			m <- .wrapped.model["learner.model"]
			f <- as.formula(paste(m$target, "~."))
			# this is stupid but kknn forces it....
			.newdata[, m$target] <- 0
			pars <- list(formula=f, train=m$data, test=.newdata)  
			pars <- c(pars, m$parset, list(...))
			m <- do.call(kknn, pars)
			if (.type=="response")
				return(m$fitted.values)
			else 
				return(m$prob)
		}
)	




