#' @include task.learn.r
#' @include tune.wrapper.r



benchmark = function(learner, task, resampling, measures, type="response", models, opts, paths) {
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
			extract = function(x) list(model=x, opt=x["opt"], path=x["path"])
		else 
			extract = function(x) list(opt=x["opt"], path=x["path"])
	} else {
		if (models)	
			extract = function(x) {list(model=x)} 
		else 
			extract = function(x) {}
	}

	
	rr = resample.fit(learner, task, resampling, extract=extract, type=type)
	result = data.frame(matrix(nrow=resampling["iters"]+1, ncol=0))
	ex = rr@extracted
	
	
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
		mods = lapply(rr@extracted, function(x) x$model)
	os = NULL
	if (opts) 
		os = lapply(rr@extracted, function(x) x$opt)
	ps = NULL
	if (paths) 
		ps = lapply(rr@extracted, function(x) x$path)
	return(list(result=result, conf.mat=cm, resample.fit=rr, models=mods, opts=os, paths=ps))
}

