#' @include control.varsel.r
roxygen()

#' @exportClass randomvarsel.control
#' @rdname randomvarsel.control 

setClass(
		"randomvarsel.control",
		contains = c("varsel.control"),
		representation = representation(
				method = "character",
				prob = "numeric"
		)
)

#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("randomvarsel.control"),
		def = function(.Object, path, same.resampling.instance, maxit, max.vars, method, prob) {
			.Object = callNextMethod(.Object, path=path, same.resampling.instance=same.resampling.instance, maxit=maxit, max.vars=max.vars)
			.Object@method = method 			
			.Object@prob = prob 			
			return(.Object)
		}
)


#' Control structure for random variable selection. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'   Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param maxit [integer] \cr 
#'   Maximal number of variable sets to evaluate. Default is 100.
#' @param method [numeric] \cr 
#'   Currently only "binomial" is implemented. Samples variables from a binomial distribution. 		        
#' @param prob [numeric] \cr 
#'   Parameter for binomial distribution. 		        
#' 		    
#' @return Control structure.
#' @exportMethod randomvarsel.control
#' @rdname randomvarsel.control 
#' @title Control structure for random variable selection. 


setGeneric(
		name = "randomvarsel.control",
		def = function(path, same.resampling.instance, maxit, method, prob) {
			if (missing(path))
				path = TRUE
      if (missing(same.resampling.instance))
        same.resampling.instance = TRUE
      if (missing(maxit))
				maxit = 100
			if (is.numeric(maxit))
				maxit = as.integer(maxit)
			if (missing(method))
				method = "binomial"
			if (missing(prob))
				prob = 0.5
			standardGeneric("randomvarsel.control")
		}
)

#' @rdname randomvarsel.control 

setMethod(
		f = "randomvarsel.control",
		signature = signature(path="logical", same.resampling.instance="logical",	maxit="integer", method="character", prob="numeric"),
		def = function(path, same.resampling.instance, maxit, method, prob) {
			new("randomvarsel.control", path=path, same.resampling.instance=same.resampling.instance, maxit=maxit, max.vars=.Machine$integer.max, method=method, prob=prob)
		}
)



