#' @include wrapped.learner.regr.r 
myrox()

#' Wrapped learner for k-Nearest Neighbor from package \code{kknn} for regression problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{k}}{Number of neighbors considered.} 	
#' 		\item{\code{distance}}{Parameter of Minkowski distance.}
#' }
#' @title kknn.regr
#' @seealso \code{\link[kknn]{kknn}}
#' @export
setClass(
		"kknn.regr", 
		contains = c("wrapped.learner.regr")
)



predict.kknn.model2 <- function(model, newdata, ...) {
}


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title kNN (regression) Constructor
setMethod(
		f = "initialize",
		signature = signature("kknn.regr"),
		def = function(.Object) {
			
			desc = new("regr.props",
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, learner.name="KKNN", learner.pack="kknn", learner.props=desc)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="kknn.regr", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, ...) {
			list(target=.targetvar, data=.data, parset=list(...))
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "kknn.regr", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, ...) {
			m <- .wrapped.model["learner.model"]
			f <- as.formula(paste(m$target, "~."))
			# this is stupid but kknn forces it....
			.newdata[, m$target] <- 0
			pars <- list(formula=f, train=m$data, test=.newdata)  
			pars <- c(pars, m$parset, list(...))
			m <- do.call(kknn, pars)
			return(m$fitted.values)
		}
)	



