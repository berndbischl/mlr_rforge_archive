resample.fit.iter <- function(learner, task, rin, i, measures, model, extract) {
	train.i = rin["train.inds"][[i]]
  test.i = rin["test.inds"][[i]]
	
	m = train(learner, task, subset=train.i)
	p = predict(m, task=task, subset=test.i)
  
  # does a measure require to calculate pred.train?
  ptrain = any(sapply(measures, function(m) m["req.pred"]))
  ms.train = rep(NA, length(measures))
  ms.test = rep(NA, length(measures))
  pred.train = NULL
  pred.test = NULL
  if (rin["predict"][i] == "train") {
    pred.train = predict(m, task, subset=train.i)
    ms.train = sapply(measures, function(pm) performance(task=task, model=m, pred=pred.train, measure=pm))
  } else if (rin["predict"][i] == "test") {
    pred.test = predict(m, task, subset=test.i)
    ms.test = sapply(measures, function(pm) performance(task=task, model=m, pred=pred.test, measure=pm))    
  } else { # "both"
    pred.train = predict(m, task, subset=train.i) 
    ms.train = sapply(measures, function(pm) performance(task=task, model=m, pred=pred.train, measure=pm))
    pred.test = predict(m, task, subset=test.i)
    ms.test = sapply(measures, function(pm) performance(task=task, model=m, pred=pred.test, measure=pm))    
  }
  
	ex = extract(m)
  list(
    measures.test = ms.test,
    measures.train = ms.train,
    model = if (model) m else NULL,  
    pred.test = pred.test,
    pred.train = pred.train,
    extract = ex
  )
}

eval.rf <- function(learner, task, resampling, measures, control, par) {

	if (is(control, "tune.control")) {
		par.vals = .mlr.scale.par(par, control)
    vars = task["input.names"]
	} else {
		par.vals = list()
    vars =  par
  }
	# todo 
#	if (control["tune.threshold"]) 
#		type = "prob"
  learner = set.hyper.pars(learner, par.vals=par.vals)
	r = resample(learner, task, resampling, measures=measures, vars=vars)
  return(r$aggr)
  
#	th = as.numeric(NA)
#	if (control["tune.threshold"]) { 
#		thr = tune.threshold(rf, measures, task, minimize=control["minimize"], thresholds=control["thresholds"])
#		rf = thr$pred
#		th = thr$th
#	}
}

