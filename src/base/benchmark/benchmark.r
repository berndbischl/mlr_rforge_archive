#' @include task.learn.r
#' @include tune.wrapper.r



benchmark = function(learner, task, resampling, measures, type="response", models, opt.pars, opt.paths) {
	if (is.character(learner)) {
		learner = make.learner(learner)
	}
	if("prob" %in% type && !learner@props@supports.probs) {
		type = setdiff(type, "prob")
		warning(paste("Learner", learner["id"], "does not support probs, won't be predicted."))
	}
	if("decision" %in% type && !learner@props@supports.decision) {
		type = setdiff(type, "decision")
		warning(paste("Learner", learner["id"], "does not support decision values, won't be predicted."))
	}
	type = union(type, "response")
	
	if (missing(measures))
		measures = default.measures(task)
	measures = make.measures(measures)
	
	if (is(learner, "tune.wrapper")) {
		if (models) 
			extract = function(x) {
				list(model=x, tuned.par=x["tuned.par"], tuned.perf=x["tuned.perf"])
			}
		else 
			extract = function(x) {
				list(tuned.par=x["tuned.par"], tuned.perf=x["tuned.perf"])
			}
		rr = resample.fit(learner, task, resampling, extract=extract, type=type)
		ex = rr@extracted
		ns = names(ex[[1]]$tuned.par)
		result = data.frame(matrix(nrow=resampling["iters"]+1, ncol=length(ns)))
		colnames(result) = ns
		for (i in 1:length(ex)) {
			result[i, ns] = ex[[i]]$tuned.par
			result[i, "tune.perf"] = ex[[i]]$tuned.perf
		}
		#replicate(length(aggr), result <<- rbind(NA, result))
		
	} else {
		if (models) 
			extract = function(x) {list(model=x)} 
		else 
			extract = function(x) {}
		result = data.frame(matrix(nrow=resampling["iters"]+1, ncol=0))
		rr = resample.fit(learner, task, resampling, type=type, extract=extract)
	}
	rp = performance(rr, measures=measures, aggr=list("combine"), task=task)
	cm = NULL
	if (is(task, "classif.task"))			
		cm = conf.matrix(rr)
	# add in combine because we cannot get that later if we throw away preds
	ms = rbind(rp$measures, rp$aggr)
	result = cbind(result, ms)
	rownames(result) = rownames(ms)
	mods = NULL
	if (models) 
		mods = lapply(rr@extracted, function(x) x@model)
	o.pars = NULL
	if (opt.pars) 
		o.pars = lapply(rr@extracted, function(x) x$tuned.par)
	o.paths = NULL
	if (opt.paths) 
		o.paths = lapply(rr@extracted, function(x) x$path)
	return(list(result=result, conf.mat=cm, resample.fit=rr, models=mods, opt.pars=o.pars, opt.paths=o.paths))
}

