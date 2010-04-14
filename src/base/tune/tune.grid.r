tune.grid <- function(learner, task, resampling, measures, aggr, control, scale) {
	ranges = control$ranges
	# if theres more than one ranges 
	if(all((names(ranges) == "ranges"))) {
		trs <- lapply(ranges, function(r) {tune.1(learner=learner, task=task, resampling=resampling, ranges=r, measures=measures, aggr=aggr, scale=scale)})
		trs2 <- lapply(1:length(ranges), function(i) make.tune.result(trs[[i]], measures, ranges[[i]]))
		ps <- lapply(trs2, function(x) x$path)
		bps <- sapply(trs2, function(x) x$perf)
		bpars <- lapply(trs2, function(x) x$par)
		i <- which.min(bps)
		perf <- Reduce(rbind.fill, ps)
		# reorder
		par.names = Reduce(union, lapply(ranges, function(x) names(x)))
		cn = colnames(perf)
		perf = perf[, c(par.names, setdiff(cn, par.names))]
		return(list(par=bpars[[i]], perf=bps[i], path = perf))
	}else {
		tr <- tune.1(learner, task, resampling, ranges, measures, aggr, scale)
		return(make.tune.result(tr, measures, ranges))
	}
}



tune.1 <- function(learner, task, resampling, ranges, measures, aggr, scale) {
	check.ranges(ranges)

	grid = expand.grid(ranges, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
	parsets <- lapply(1:nrow(grid), function(i) as.list(grid[i,,drop=FALSE]))	
	
#	if (.ps$mode %in% c("snowfall", "sfCluster")) {
#		sfExport("learner")
#		sfExport("task")
#		sfExport("resample.instance")
#		if (.ps$level == "tune") {
#			sfExport("parsets")
#			sfExport("measure")
#		}
#	} 
	
	
	perf = eval.parsets(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, pars=parsets, ps.scale=scale, ps.names=names(ranges))
	return(perf)
}

make.tune.result <- function(perf, measures, ranges) {
	n = length(ranges)
	best.i = which.min(perf[, n+1])
	best.parameters <- perf[best.i, 1:n, drop=F]
	best.performance <- perf[best.i, n+1] 
	return(list(par=best.parameters, perf=best.performance, path = perf))
}




