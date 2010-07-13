#' @include control.tune.r
roxygen()

#' @exportClass optim.control
#' @rdname optim.control 

setClass(
		"optim.control",
		contains = c("tune.control")
)


#' Control structure for Nelder-Mead tuning. 
#' 
#' See \code{\link[stats]{optim}} for details of Nelder-Mead settings.
#' 
#' @param start [numeric] \cr
#'    Named vector of initial values.
#' @param minimize [logical] \cr
#'       Minimize performance measure? Default is TRUE. 
#' @param scale [\code{\link{function}}] \cr
#'        A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a single, numerical vector and return a scaled one. Default is identity function.
#' @param ... Passed down to optim.
#' 
#' @return Control structure for tuning.
#' @export 
#' @title Control for Nelder-Mead tuning.

#' Control structure for grid search tuning. 
#' 
#' @param ranges [\code{\link{list}}] \cr 
#' 		A list of named vectors/lists of possible values for each hyperparameter. 
#'      You can also pass a list of such ranges by using [\code{\link{combine.ranges}}] 
#'      in the rare case when it does not make sense to search a complete cross-product of range values.     
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE. 
#' @param tune.threshold [logical] \cr 
#'       Perform empirical thresholding? Default is FALSE. Only supported for binary classification and you have to set predict.type to "prob" for this in make.learner. 
#' @param thresholds [numeric] \cr 
#'		Number of thresholds to try in tuning. Predicted probabilities are sorted and divided into groups of equal size. Default is 10. 		        
#' @param scale [\code{\link{function}}] \cr 
#'        A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a vector and return a scaled one. Default is identity function.
#' 		    
#' @return Control structure for tuning.
#' @exportMethod optim.control
#' @rdname optim.control 
#' @title Control for grid search tuning. 


setGeneric(
		name = "optim.control",
		def = function(minimize, tune.threshold, thresholds, start, lower, upper, scale, ...) {
			if (missing(minimize))
				minimize=TRUE
			if (missing(tune.threshold))
				tune.threshold=FALSE
			if (missing(thresholds))
				thresholds=10
			if (is.numeric(thresholds))
				thresholds = as.integer(thresholds)
			if (missing(start))
				stop("You have to provide a start value!")
			if (missing(lower))
				{lower=start;lower[]=-Inf}	
			if (length(lower)==1)
				lower = rep(lower, length(start))
			if (is.null(names(lower)))
				names(lower) = names(start)
			if (missing(upper))
				{upper=start;upper[]=Inf}				
			if (length(upper)==1)
				upper = rep(upper, length(start))
			if (is.null(names(upper)))
				names(upper) = names(start)
			if (missing(scale))
				scale=identity
			standardGeneric("optim.control")
		}
)


#' @rdname optim.control 

setMethod(
		f = "optim.control",
		signature = signature(minimize="logical", tune.threshold="logical", thresholds="integer", start="numeric", lower="numeric", upper="numeric", scale="function"),
		def = function(minimize, tune.threshold, thresholds, start, lower, upper, scale, ...) {
			new("optim.control", minimize=minimize, tune.threshold=tune.threshold, thresholds=thresholds,
					start=as.list(start), lower=as.list(lower), upper=as.list(upper), ranges=list(), partypes=character(0), scale=scale, ...)
		}
)
