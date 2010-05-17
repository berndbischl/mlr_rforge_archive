
#' @export
tune.threshold = function(pred, measures, aggr, task, minimize=T, thresholds=10) {
	if (missing(measures))
		measures = default.measures(pred@task.desc)
	measures = make.measures(measures)
	if (missing(aggr))
		aggr = default.aggr(pred@task.desc)
	aggr = make.aggrs(aggr)
	
	pos = pred@task.desc["positive"]
	neg = pred@task.desc["negative"]
	levs = pred@data.desc["class.levels"]
	probs = pred["prob"]
	if (is.null(probs))
		stop("No probs in prediction! Maybe you forgot type='prob'?")
	f = function(x) {
		labels = prob.threshold(probs=probs, pos=pos, neg=neg, levels=levs, threshold=x)
		pred@df$response = labels
		perf = performance(pred, measures=measures, aggr=aggr, task=task)
		return(perf$aggr[1,1])
	}
	probs.sorted = sort(unique(probs))
	len = min(thresholds, length(probs.sorted))
	probs.seq = probs.sorted[seq(1, length(probs.sorted), length=len)]
	vals = sapply(probs.seq, f)
	if (minimize)
		j = which.min(vals)
	else
		j = which.max(vals)
	th = probs.seq[j]
	labels = prob.threshold(probs=probs, pos=pos, neg=neg, levels=levs, threshold=th)
	pred@df$response = labels
	return(list(th=th, pred=pred, th.seq=probs.seq, perf=vals))
}
