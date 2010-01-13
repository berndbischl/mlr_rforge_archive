
forward.sel <- function(learn.task, resample.instance, parset, measure) {
	vars = c()
	result = data.frame(var="x", perf=-1, stringsAsFactors=F)
	while(length(vars) < length(learn.task["input.names"])) {
		res = forward.sel.1(learn.task, resample.instance, parset,measure, vars)
		vars = c(vars, res$var)
		result = rbind(result, res)
		print(result)
	}
}



forward.sel.1 <- function(learn.task, resample.instance, parset, measure, vars) {
	is = learn.task["input.names"]
	y = learn.task["target.name"]
	remaining = setdiff(is, vars)
	best.perf = 9999999
	best.var = ""
	for(v in remaining) {
		print(v)
		vars2 = c(vars, v)
		t2 <- learn.task
		t2@data <- learn.task["data", select=c(vars2,y)]
		print(system.time(rf <- resample.fit(t2, resample.instance=resample.instance, parset=parset)))
		perf <- resample.performance(rf, measure=measure)
		if (perf$mean < best.perf) {
			best.perf <- perf$mean
			best.var <- v
		}
	}
	return(list(var=best.var, perf=best.perf))
}