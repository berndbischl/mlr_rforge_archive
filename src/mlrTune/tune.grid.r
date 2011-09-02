tune.grid <- function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  if (!all(sapply(par.set@pars, function(x) x@type %in% c("discrete", "logical"))))
    stop("Grid search can only be applied to discrete and logical parameters!")
  # drop names from par.set
  vals = values(par.set, only.names=TRUE) 
  grid = expand.grid(vals, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  vals = lapply(seq(length=nrow(grid)), function(i) as.list(grid[i,,drop=FALSE]))
  vals = lapply(vals, function(val) par.valnames.to.vals(val, par.set))
  eval.states(learner, task, resampling, measures, par.set, NULL, control, opt.path, vals)
  i = getBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getPathElement(opt.path, i)
  new("OptResult", learner, control, e$x, e$y, opt.path)
}




