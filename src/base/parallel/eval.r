resample.fit.iter <- function(learner, task, rin, vars, i, measures, prediction, model, extract) {
	train.i = get.train.set(rin, i)
	ts = get.test.set(rin, i)
	test.i = ts$inds
	g = ts$group
	
	m = train(learner, task, subset=train.i, vars=vars)
	p = predict(m, task=task, subset=test.i, group=g)
  
  # does a measure require to calculate pred.train?
  ptrain = any(sapply(measures, function(m) m["req.pred.train"]))
  if (ptrain) {
    pred.train = predict(m, task, subset=train.i, group=g) 
    ms = sapply(measures, function(pm) performance(task=task, model=m, pred=p, pred.train=pred.train, measure=pm))
  } else {
    pred.train = NULL
    ms = sapply(measures, function(pm) performance(task=task, model=m, pred=p, measure=pm))
  }
	ex = lapply(extract, function(f) f(pm))
  list(
    measures = ms,
    model = if (model) m else NULL,  
    pred.test = if (prediction) p else NULL,
    pred.train = if (prediction) pred.train else NULL,
    extract = ex
  )
}

eval.rf <- function(learner, task, resampling, measures, aggr, control, par) {

	if (is(control, "tune.control")) {
		par.vals = .mlr.scale.par(par, control)
		vars = task["input.names"]
	} else {
		par.vals = list()
		vars = par
	}
	# todo 
#	if (control["tune.threshold"]) 
#		type = "prob"
	learner = set.hyper.pars(par.vals)
	r = resample(learner, task, resampling, vars=vars)
	th = as.numeric(NA)
	if (control["tune.threshold"]) { 
		thr = tune.threshold(rf, measures, aggr, task, minimize=control["minimize"], thresholds=control["thresholds"])
		rf = thr$pred
		th = thr$th
	}
	perf = performance(rf, measures=measures, aggr=aggr, task=task)
	list(perf=perf, th=th)
}

