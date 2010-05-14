
resample.fit.iter <- function(learner, task, rin, parset, vars, i, extract) {
	train.i <- rin["train.inds", i]
	test.i <- rin["test.inds", i]
	m = train(learner, task, subset=train.i, parset=parset, vars=vars)
	p = predict(m, task=task, subset=test.i)
	# faster for parallel
	ex = extract(m)
	return(list(pred=p, extracted=ex))	
}

eval.rf <- function(learner, task, resampling, measures, aggr, control, par) {

	if (is(control, "tune.control")) {
		parset = scale.par(par, control)
		vars = task["input.names"]
	} else {
		parset = list()
		if (is.null(par)) {
			vars = task["input.names"]
		}
	}
	# todo 
#	if (control["tune.threshold"]) 
#		type = "prob"
	
	rf = resample.fit(learner, task, resampling, parset=parset, vars=vars)

	th = as.numeric(NA)
	if (control["tune.threshold"]) { 
		thr = tune.threshold(rf, measures, aggr, task, minimize=control["minimize"], thresholds=control["thresholds"])
		rf = thr$pred
		th = thr$th
	}
	perf = performance(rf, measures=measures, aggr=aggr, task=task)
	list(perf=perf, th=th)
}

