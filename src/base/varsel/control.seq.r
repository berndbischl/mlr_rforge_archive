#' @include control.varsel.r
roxygen()

setClass(
		"seq.control",
		contains = c("varsel.control"),
		representation = representation(
				alpha = "numeric", 
				beta = "numeric" 
		)
)


setMethod(
		f = "initialize",
		signature = signature("seq.control"),
		def = function(.Object, method, minimize, tune.threshold, thresholds, maxit, max.vars, alpha, beta) {
			.Object = callNextMethod(.Object, method, minimize, tune.threshold, thresholds, maxit=maxit, max.vars)
			.Object@alpha = alpha 			
			.Object@beta = beta 			
			return(.Object)
		}
)


#' Control structure for sequential variable selection. 
#' 
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE.
#' @param maxit [integer] \cr 
#'       Maximal number of variable sets to evaluate. Default is 100.
#' @param max.vars [integer] \cr 
#'       Maximal number of allowed variables in the final set. Default is max. integer.
#' @param alpha [numeric] \cr 
#'  	 sfs, sffs: In a forward step, minimal improvement of performance measure. Can be negative.        
#' @param beta [numeric] \cr 
#'  	 sbs, sfbs: In a backward step, minimal improvement of performance measure. Can be negative.        
#' @param tune.threshold [logical] \cr 
#'		Perform empirical thresholding? Default is FALSE. Only supported for binary classification and you have to set predict.type to "prob" for this in make.learner. 
#' @param thresholds [numeric] \cr 
#'		Number of thresholds to try in tuning. Predicted probabilities are sorted and divided into groups of equal size. Default is 10. 		        
#' 		    
#' @return Control structure.
#' @export
#' @rdname seq.control 
#' @title Control structure for sequential variable selection. 

seq.control = function(minimize, tune.threshold, thresholds, maxit, max.vars, alpha, beta) {
			if (missing(minimize))
				minimize=TRUE
			if (missing(tune.threshold))
				tune.threshold=FALSE
			if (missing(thresholds))
				thresholds=10
			if (missing(maxit))
				maxit = .Machine$integer.max
			if (missing(max.vars))
				max.vars = .Machine$integer.max
			if (missing(alpha))
				alpha=0.01
			if (missing(beta))
				beta=0.01
	new("seq.control", method="seq", minimize=minimize, 
			tune.threshold=tune.threshold, thresholds=thresholds, 
			maxit=maxit, max.vars=max.vars, alpha=alpha, beta=beta)
}




#setGeneric(
#		name = "seq.control",
#		def = function(minimize, tune.threshold, thresholds, maxit, max.vars, alpha, beta) {
#			if (missing(minimize))
#				minimize=TRUE
#			if (missing(tune.threshold))
#				tune.threshold=FALSE
#			if (missing(thresholds))
#				thresholds=10
#			if (missing(maxit))
#				maxit = .Machine$integer.max
#			if (missing(max.vars))
#				max.vars = .Machine$integer.max
#			if (missing(alpha))
#				alpha=0.01
#			if (missing(beta))
#				beta=0.01
#			standardGeneric("seq.control")
#		}
#)
#
#
#
#setMethod(
#		f = "seq.control",
#		signature = signature(minimize="logical", tune.threshold="logical", thresholds="numeric", 
#				maxit="numeric", max.vars="numeric", alpha="numeric", beta="numeric"),
#		def = function(minimize, tune.threshold, thresholds, maxit, max.vars, alpha, beta) {
#			new("seq.control", method="seq", minimize=minimize, 
#					tune.threshold=tune.threshold, thresholds=thresholds, 
#					maxit=maxit, max.vars=max.vars, alpha=alpha, beta=beta)
#		}
#)



