#' Optimizes the threshold of prediction based on probabilities.
#' Currently only implemented for probabilities and binary classification. 
#' 
#' 
#' @param pred [\code{\linkS4class{Prediction}}] \cr
#'   Prediction object to use for tuning the treshold.
#' @param measure [\code{\linkS4class{Measure}}]\cr
#'   Performance measure to optimize. 
#' @param task [\code{\linkS4class{LearnTask}}] \cr
#'   Learning task. Rarely neeeded, only when required for the performance measure. 
#' @param thresholds [\code{integer}] \cr
#'   Number of thresholds to try in tuning.  	
#' 
#' @return A list with with the following components: "th" is the optimal threshold, pred a prediction object based on "th", 
#' 		  		"th.seq" a numerical vector of threhold values which were tried and "perf" their respective performance values.  	 
#'
#' @export
#' @seealso \code{\link{tune}}
#' @title Tune prediction threshold.

tuneThreshold = function(pred, measure, task, model, thresholds=100) {
  td = pred@task.desc
	if (missing(measure))
		measure = mlr:::default.measures(td)[[1]]
  probs = getProb(pred)
  
  if (is.null(probs))
    stop("No probs in prediction! Maybe you forgot type='prob'?")
  # brutally return NA if we find any NA in the pred. probs...
  if (any(is.na(probs))) {
    return(list(th=NA, pred=pred, th.seq=numeric(0), perf=numeric(0)))
  }
  sig = ifelse(measure@minimize,1,-1)
	f = function(x) {
    pred2 = setThreshold(pred, x)
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
