#' @include VarselControl.R
roxygen()

#' @exportClass VarselControlRandom
#' @rdname VarselControl 

setClass(
		"VarselControlRandom",
		contains = c("VarselControl"),
		representation = representation(
				method = "character",
				prob = "numeric"
		)
)

#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("VarselControlRandom"),
		def = function(.Object, path, same.resampling.instance, maxit, max.vars, method, prob) {
			.Object = callNextMethod(.Object, path=path, same.resampling.instance=same.resampling.instance, maxit=maxit, max.vars=max.vars)
			.Object@method = method 			
			.Object@prob = prob 			
			return(.Object)
		}
)


#' Control structure for random variable selection. 
#'
#' Currently 0/1 values are sampled from a binomial distribution to decide whether a variable
#' is included in the model.   
#' 
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'   Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param maxit [\code{integer}] \cr 
#'   Maximal number of variable sets to evaluate. Default is 100.
#' @param method [numeric] \cr 
#'   Currently only "binomial" is implemented. Samples variables from a binomial distribution. 		        
#' @param prob [numeric] \cr 
#'   Parameter for binomial distribution. 		        
#' 		    
#' @return Control structure.
#' @exportMethod makeVarselControlRandom
#' @title Control structure for random variable selection. 


setGeneric(
		name = "makeVarselControlRandom",
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
			standardGeneric("makeVarselControlRandom")
		}
)

#' @rdname makeVarselControlRandom 

setMethod(
		f = "makeVarselControlRandom",
		signature = signature(path="logical", same.resampling.instance="logical",	maxit="integer", method="character", prob="numeric"),
		def = function(path, same.resampling.instance, maxit, method, prob) {
			new("VarselControlRandom", path=path, same.resampling.instance=same.resampling.instance, maxit=maxit, max.vars=.Machine$integer.max, method=method, prob=prob)
		}
)



