#' @include task.learn.r
#' @include tune.wrapper.r



benchmark = function(learner, task, resampling, measures, type="response") {
	if (is.character(learner)) {
		learner = make.learner(learner, task)
	}
	if("prob" %in% type && !learner@learner.props@supports.probs) {
		type = setdiff(type, "prob")
		warning(paste("Learner", learner@learner.name, "does not support probs, won't be predicted."))
	}
	if("decision" %in% type && !learner@learner.props@supports.decision) {
		type = setdiff(type, "decision")
		warning(paste("Learner", learner@learner.name, "does not support decision values, won't be predicted."))
	}
	type = union(type, "response")
	
	if (missing(measures))
		measures = default.measures(task)
	measures = make.measures(measures)
	
	if(task["name"] == "" || is.null(task["name"]) || is.null(task["name"]))
		stop("Every task in a benchmark experiment has to be named!")
	
	if (is(learner, "tune.wrapper")) {
		extract = function(x) {
			list(tuned.par=x["tuned.par"], tuned.perf=x["tuned.perf"])
		}
		rr <- resample.fit(learner, task, resampling, extract=extract, type=type)
		ex = rr@extracted
		ns = names(ex[[1]]$tuned.par)
		result = data.frame(matrix(nrow=resampling["iters"], ncol=length(ns)))
		colnames(result) = ns
		for (i in 1:length(ex)) {
			result[i, ns] = ex[[i]]$tuned.par
			result[i, "tune.perf"] = ex[[i]]$tuned.perf
		}
		#replicate(length(aggr), result <<- rbind(NA, result))
		
	} else {
		result = data.frame(matrix(nrow=resampling["iters"], ncol=0))
		rr <- resample.fit(learner, task, resampling, type=type)
	}
	rp = performance(rr, measures=measures, aggr=list())
	cm = NA
	if (is(task, "classif.task"))			
		cm = conf.matrix(task, rr)
	ms = rp$measures
	result = cbind(result, ms)
	rownames(result) = rownames(ms)
	return(list(result=result, conf.mat=cm, resample.fit=rr))
}

