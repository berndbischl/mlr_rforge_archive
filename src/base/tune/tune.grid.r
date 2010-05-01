tune.grid <- function(learner, task, resampling, measures, aggr, control) {
	ranges = control$ranges
	# if theres more than one ranges 
	if(all((names(ranges) == "ranges"))) {
		ors = lapply(ranges, function(r) {tune.1(learner, task, resampling, ranges, measures, aggr, control)})
		
		ps = lapply(ors, function(x) x@path)
		ps = Reduce(c, ps)
		perfs = sapply(ors, function(x) x@opt$perf[1])
		if (control$minimize)
			i = which.min(perfs)
		else				
			i = which.max(perfs)
		new("opt.result", opt=ors[[i]]@opt,  path=ps)
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




