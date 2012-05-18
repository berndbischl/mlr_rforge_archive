tuneGrid = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  # drop names from par.set
  vals = getValues(par.set) 
  inds = lapply(vals, seq_along)
  grid = expand.grid(inds)
  vals = lapply(seq_len(nrow(grid)), function(i) {
    val.inds = as.numeric(grid[i,])
    Map(function(v, j) v[[j]], vals, val.inds)
  })  
  evalOptimizationStates(learner, task, resampling, measures, par.set, NULL, control, opt.path, log.fun, vals, dobs=1L, eols=1L)

  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
  new("OptResult", learner, control, e$x, e$y, opt.path)
}




