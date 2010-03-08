#' @include task.learn.r
#' @include tune.wrapper.r

setGeneric(
		name = "benchmark",
		def = function(learner, task, resampling) {
			if (is.character(learner)) {
				learner = make.learner(learner, task)
			}
			standardGeneric("benchmark")
		}
)

setMethod(
		f = "benchmark",
		
		signature = c(learner="wrapped.learner", task="learn.task", resampling="resample.instance"),
		
		def = function(learner, task, resampling) {
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
			} else {
				result = data.frame(matrix(nrow=resampling["iters"], ncol=0))
				rr <- resample.fit(learner, task, resampling)
			}
			rp = resample.performance(task, rr)
			cm = NA
			if (is(task, "classif.task"))			
				cm = conf.matrix(task, rr)
			result[, "test.perf"] = rp$aggr2
			return(list(result=result, conf.mat=cm))
		}
)


