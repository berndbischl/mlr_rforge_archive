tune.grid <- function(learner, task, resampling, measure, bounds, control) {
  if (any(sapply(bounds@pars, function(x) x@type != "discrete")))
    stop("Grid search can only be applied to discrete parameters!")
  # todo: should we really do this? or allow both possibilities? what about wrapper?
	# convert to instance so all pars are evaluated on the same splits
	if (is(resampling, "resample.desc")) 
		resampling = make.res.instance(resampling, task=task)
  # drop names from par.descs
  ranges = lapply(control["par.descs"], function(y) unlist(y["vals", names=FALSE])) 
  names(ranges) = control["par.names"]	
	tune.1(learner, task, resampling, ranges, measures, control)
}



tune.1 <- function(learner, task, resampling, ranges, measures, control) {
  ns = names(ranges)
  if(any(is.na(ns) | ns == "")) {
    stop("All element of a ranges list have to be named!")
  }
	
	# todo: make this better 
	if (length(ranges) == 0) {
		bs = eval.state.tune(learner=learner, task=task, resampling=resampling,  
				measures=measures, control=control, 
				par=list(), event="grid")
		path = add.path.tune(list(), bs, T)	
	} else {
		grid = expand.grid(ranges, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
		
		parsets = lapply(seq(length=nrow(grid)), function(i) as.list(grid[i,,drop=FALSE]))	
		es = eval.states.tune(learner=learner, task=task, resampling=resampling,  
				measures=measures, control=control, 
				pars=parsets, event="grid")
		
		bs = select.best.state(es, measures[[1]])
		path = add.path.els.tune(path=list(), ess=es, best=bs)
	}
	new("opt.result", control=control, opt=make.path.el(bs), path=path)
}




