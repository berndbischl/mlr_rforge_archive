tune.grid <- function(learner, task, resampling, measures, aggr, control) {
	ranges = control$ranges
	# if theres more than one ranges 
	if(all((names(ranges) == "ranges"))) {
		trs <- lapply(ranges, function(r) {tune.1(learner=learner, task=task, resampling=resampling, ranges=r, measures=measures, aggr=aggr)})
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
		tune.1(learner, task, resampling, ranges, measures, aggr, control)
	}
}



tune.1 <- function(learner, task, resampling, ranges, measures, aggr, control) {
	check.ranges(ranges)

	grid = expand.grid(ranges, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
	parsets = lapply(1:nrow(grid), function(i) as.list(grid[i,,drop=FALSE]))	

	es = eval.states.tune(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, 
			pars=parsets, ps.scale=control$scale, ps.names=names(ranges), event="grid")
	
	bs = select.best.state(es, control)
	path = add.path.els.tune(path=list(), ess=es, best=bs)
	new("opt.result", opt=make.path.el(bs),  path=path)

#	if (.ps$mode %in% c("snowfall", "sfCluster")) {
#		sfExport("learner")
#		sfExport("task")
#		sfExport("resample.instance")
#		if (.ps$level == "tune") {
#			sfExport("parsets")
#			sfExport("measure")
#		}
#	} 
}




