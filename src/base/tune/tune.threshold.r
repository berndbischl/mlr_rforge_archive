#' Optimizes the threshold of prediction based on probabilities or decision values.
#' Currently only implemented for probabilities and binary classification. 
#' 
#' 
#' @param pred [\code{\linkS4class{prediction}}] \cr
#' 		  Prediction object to use for tuning the treshold.
#' @param measures [see \code{\link{measures}}]
#'        Performance measures.
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task. Rarely neeeded, only when required for the performance measure. 
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE.
#' @param thresholds [integer] \cr
#' 		  Number of thresholds to try in tuning.  	
#' 
#' @return A list with with the following components: "th" is the optimal threshold, pred a prediction object based on "th", 
#' 		  		"th.seq" a numerical vector of threhold values which were tried and "perf" their respective performance values.  	 
#'
#' @export
#' @seealso \code{\link{tune}}
#' @title Tune prediction threshold.

tune.threshold = function(pred, measure, task, model, thresholds=100) {
  td = pred["desc"]
	if (missing(measures))
		measure = default.measures(td)[[1]]
  probs = pred["prob"]
  
  if (is.null(probs))
    stop("No probs in prediction! Maybe you forgot type='prob'?")
  # brutally return NA if we find any NA in the pred. probs...
  if (any(is.na(probs))) {
    return(list(th=NA, pred=pred, th.seq=numeric(0), perf=numeric(0)))
  }
  sig = ifelse(measure["minimize"],1,-1)
	f = function(x) {
    pred2 = set.threshold(pred, x)
		sig*performance(pred, measure, task, model)
	}
	probs.sorted = sort(unique(probs))
	len = min(thresholds, length(probs.sorted))
	probs.seq = probs.sorted[seq(1, length(probs.sorted), length=len)]
	vals = sapply(probs.seq, f)
  j = which.min(vals)
	th = probs.seq[j]
	labels = prob.threshold(probs=probs, pos=pos, neg=neg, levels=levs, threshold=th)
	pred@df$response = labels
	return(list(th=th, pred=pred, th.seq=probs.seq, vals=vals))
}
