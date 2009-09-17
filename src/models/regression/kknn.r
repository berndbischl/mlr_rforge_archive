#' @include wrapped.learner.regr.r 
roxygen()

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


#----------------- train.kknn.model ---------------------------------------------------------

train.kknn.model2 <- function(formula, data, ...) {
	model <- list(formula=formula, data=data, parset=list(...))
	class(model) <- "kknn"
	return(model)
}

predict.kknn.model2 <- function(model, newdata, ...) {
	# this is stupid but kknn forces it....
	cl <- as.character(model$formula)[2]
	newdata[,cl] <- 0
	
	pars <- list(formula=model$formula, train=model$data, test=newdata)  
	pars <- c(pars, model$parset)
	do.call(kknn, pars)$fitted.values
}


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title kNN (regression) Constructor
#' @rdname kknn.regr-class

setMethod(
		f = "initialize",
		signature = signature("kknn.regr"),
		def = function(.Object) {
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.weights = FALSE
			)
			
			.Object <- callNextMethod(.Object, learner.name="KKNN", learner.pack="kknn",
					train.fct=train.kknn.model2, predict.fct=predict.kknn.model2,
					learner.props=desc)
			
			return(.Object)
		}
)



