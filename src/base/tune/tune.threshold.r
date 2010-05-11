

tune.threshold = function(pred, measures, aggr, task, minimize) {
	f = function(x) {
		pred = prob.threshold.pred(pred, threshold=x)
		perf = performance(pred, measures=measures, aggr=aggr, task=task)
		return(perf$aggr[1,1])
	}
	th = optimize(f, interval=c(0,1), maximum=!minimize)[[1]]
	pred = prob.threshold.pred(pred, threshold=th)
	return(list(th=th, pred=pred))
}
