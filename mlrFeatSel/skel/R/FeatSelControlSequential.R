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
#' @return Control structure.
#' @export
#makeFeatSelControlSequential = function(same.resampling.instance=TRUE, method, alpha=0.01, beta=0.01,
#  maxit=100L, max.features=as.integer(NA), prob=0.5) {
#  
#  ctrl = makeFeatSelControl(same.resampling.instance=same.resampling.instance, 
#                            maxit=maxit, max.features=max.features, cl="FeatSelControlRandom")
#  ctrl$method = method
#  return(ctrl)
#}
#
#				method = "character", 
#				alpha = "numeric", 
#				beta = "numeric" 
#		)
#)


#setGeneric(
#		name = "makeVarselControlSequential",
#		def = function(path, same.resampling.instance, max.vars, method, alpha, beta) {
#			if (missing(path))
#				path = TRUE
#      if (missing(same.resampling.instance))
#        same.resampling.instance = TRUE
#      if (missing(max.vars))
#				max.vars = .Machine$integer.max
#			if (is.numeric(max.vars))
#				max.vars = as.integer(max.vars)
#			if (missing(method))
#				method="sfs"			
#			standardGeneric("makeVarselControlSequential")
#		}
#)
