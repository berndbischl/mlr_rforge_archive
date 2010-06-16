#' @include task.learn.r
#' @include opt.wrapper.r



benchmark = function(learner, task, resampling, measures, models, paths) {
	if (is.character(learner)) {
		learner = make.learner(learner)
	}
	
	if (missing(measures))
		measures = default.measures(task)
	measures = make.measures(measures)
	
	if (is(learner, "opt.wrapper")) {
		learner@control@path = paths
		if (models) 
			extract = function(x) list(model=x, or=x["opt.result"])
		else 
			extract = function(x) list(or=x["opt.result"])
	} else {
		if (models)	
			extract = function(x) {list(model=x)} 
		else 
			extract = function(x) {}
	}

	
	rr = resample.fit(learner, task, resampling, extract=extract)
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
	ors = NULL
	ors = lapply(rr@extracted, function(x) x$or)
	return(list(result=result, conf.mat=cm, resample.fit=rr, models=mods, ors=ors))
}

