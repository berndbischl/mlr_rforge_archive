#' @include task.learn.r
#' @include tune.wrapper.r

setGeneric(
		name = "benchmark",
		def = function(learner, task, resampling, measures, aggr) {
			if (is.character(learner)) {
				learner = make.learner(learner, task)
			}
			if (missing(measures))
				measures = default.measures(task)
			measures = make.measures(measures)
			if (missing(aggr))
				aggr = default.aggr(task)
			
			standardGeneric("benchmark")
		}
)

setMethod(
		f = "benchmark",
		
		signature = c(learner="wrapped.learner", task="learn.task", resampling="resample.instance", measures="list", aggr="list"),
		
		def = function(learner, task, resampling, measures, aggr) {
			if (is(learner, "tune.wrapper")) {
				extract = function(x) {
					list(tuned.par=x["tuned.par"], tuned.perf=x["tuned.perf"])
				}
				rr <- resample.fit(learner, task, resampling, extract=extract)
				ex = rr@extracted
				ns = names(ex[[1]]$tuned.par)
				result = data.frame(matrix(nrow=resampling["iters"], ncol=length(ns)))
				colnames(result) = ns
				for (i in 1:length(ex)) {
					result[i, ns] = ex[[i]]$tuned.par
					result[i, "tune.perf"] = ex[[i]]$tuned.perf
				}
				replicate(length(aggr), result <<- rbind(NA, result))
				
			} else {
				result = data.frame(matrix(nrow=resampling["iters"]+length(aggr), ncol=0))
				rr <- resample.fit(learner, task, resampling)
			}
			rp = resample.performance(task, rr, measures=measures, aggr=aggr)
			cm = NA
			if (is(task, "classif.task"))			
				cm = conf.matrix(task, rr)
			ms = rp$measures
			result = cbind(result, ms)
			rownames(result) = rownames(ms)
			return(list(result=result, conf.mat=cm))
		}
)


