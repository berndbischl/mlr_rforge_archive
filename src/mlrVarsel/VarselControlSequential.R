#' @include VarselControl.R
roxygen()

#' Control structure for sequential variable selection. 
#' @exportClass VarselControlSequential
setClass(
		"VarselControlSequential",
		contains = c("VarselControl"),
		representation = representation(
				method = "character", 
				alpha = "numeric", 
				beta = "numeric" 
		)
)

#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("VarselControlSequential"),
		def = function(.Object, path, same.resampling.instance, max.vars, method, alpha, beta) {
			.Object = callNextMethod(.Object, path=path, same.resampling.instance=same.resampling.instance, 
					maxit=.Machine$integer.max, max.vars=max.vars)
			.Object@alpha = alpha 			
			.Object@beta = beta 	
			.Object@method = method 			
			return(.Object)
		}
)


#' Control structure for sequential variable selection. 
#' 
#' sfs:\cr 
#' Sequential forward search. Starts from the bit vector with all 0s. In each iteration all bit vectors with one
#' more bit switched on are evaluated and the best one is selected. Stops when no signifanct improvement is made
#' or too many bits have been selected.\cr     
#' sbs:\cr 
#' Sequential backward search. Starts from the bit vector with all 1s. In each iteration all bit vectors with one
#' less bit switched on are evaluated and the best one is selected. Stops when no signifanct improvement is made
#' and enough bits have been swichted off.\cr     
#' sffs:\cr 
#' Sequential floating forward search. Same as sfs, but after each forward step one removal step is allowed. 
#' Stops when two subsequent steps failed.\cr   
#' sfbs:\cr 
#' Sequential floating forward search. Same as sbs, but after each backward step one forward step is allowed. 
#' Stops when two subsequent steps failed.\cr   
#'   
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param max.vars [\code{integer}] \cr 
#'   Maximal number of allowed variables in the final set. Default is max. integer.
#' @param method [\code{\link{character}}] \cr
#'   Search method. 'sfs', 'sbs', 'sffs' or 'sfbs', see details. Default is "sfs".    
#' @param alpha [numeric] \cr 
#'   sfs, sffs: In a forward step, minimal improvement of performance measure. Can be negative.        
#' @param beta [numeric] \cr 
#'   sbs, sfbs: In a backward step, minimal improvement of performance measure. Can be negative.        
#' 		    
#' @return Control structure.
#' @exportMethod makeVarselControlSequential
#' @rdname makeVarselControlSequential 
#' @title Control structure for sequential variable selection. 


setGeneric(
		name = "makeVarselControlSequential",
		def = function(path, same.resampling.instance, max.vars, method, alpha, beta) {
			if (missing(path))
				path = TRUE
      if (missing(same.resampling.instance))
        same.resampling.instance = TRUE
      if (missing(max.vars))
				max.vars = .Machine$integer.max
			if (is.numeric(max.vars))
				max.vars = as.integer(max.vars)
			if (missing(method))
				method="sfs"			
			if (missing(alpha))
				alpha=0.01
			if (missing(beta))
				beta=0.01
			standardGeneric("makeVarselControlSequential")
		}
)


#' @rdname makeVarselControlSequential 

setMethod(
		f = "makeVarselControlSequential",
		signature = signature(path="logical",	same.resampling.instance="logical", max.vars="integer", method="character", alpha="numeric", beta="numeric"),
		def = function(path, same.resampling.instance, max.vars, method, alpha, beta) {
			new("VarselControlSequential", path=path, same.resampling.instance=same.resampling.instance, max.vars=max.vars, method=method, alpha=alpha, beta=beta)
		}
)



