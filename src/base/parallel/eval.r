
resample.fit.iter <- function(learner, task, rin, parset, vars, type, threshold, i, extract) {
	train.i <- rin["train.inds", i]
	test.i <- rin["test.inds", i]
	m <- train(learner, task, subset=train.i, parset=parset, vars=vars)
	if (is(task, "classif.task"))
		p <- predict(m, task=task, subset=test.i, type=type, threshold=threshold)
	else 
		p <- predict(m, task=task, subset=test.i)
	# faster for parallel
	ex = extract(m)
	return(list(pred=p, extracted=ex))	
}

eval.rf <- function(learner, task, resampling, type, measures, aggr, control, par) {

	if (is(control, "tune.control")) {
		parset = scale.par(par, control)
		vars = task["input.names"]
	} else {
		parset = list()
		if (is.null(par)) {
			vars = task["input.names"]
		}
	}
	
	if (control["tune.threshold"]) 
		type = "prob"
	
	rf <- resample.fit(learner, task, resampling, type=type, parset=parset, vars=vars)

	if (control["tune.threshold"]) { 
		th = tune.threshold(rf, measures, aggr, task, minimize=control["minimize"], thresholds=control["thresholds"])
		rf = th$pred
	}
	perf = performance(rf, measures=measures, aggr=aggr, task=task)
	list(perf=perf, th=th$th)
}

