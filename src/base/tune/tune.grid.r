tune.grid <- function(learner, task, resampling, measure, bounds, control, opt.path, logger) {
  if (!all(sapply(bounds@pars, function(x) x@type %in% c("discrete", "logical"))))
    stop("Grid search can only be applied to discrete and logical parameters!")
  # todo: should we really do this? or allow both possibilities? what about wrapper?
  # convert to instance so all pars are evaluated on the same splits
  if (is(resampling, "resample.desc")) 
    resampling = make.res.instance(resampling, task=task)
  # drop names from par.descs
  vals = values(bounds, only.names=TRUE) 
  
  grid = expand.grid(vals, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  parsets = lapply(seq(length=nrow(grid)), function(i) as.list(grid[i,,drop=FALSE]))
  es = eval.states(learner, task, resampling, measure, bounds, control, parsets)
  
  bs = select.best.state(es, measure)
  path = add.path.els.tune(path=list(), ess=es, best=bs)
  
  new("opt.result", control=control, opt=make.path.el(bs), path=path)
}




