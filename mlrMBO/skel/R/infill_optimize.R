# Optimizers for infill criteria

# General interface
#
# @param infill.crit [\code{function}]\cr 
#   Infill criterion function.
# @param design [\code{data.frame}]\cr 
#   Design of already visited points.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @return [\code{data.frame}]. One proposed point that should be evaluated.

# mean response of model
infillOptDesign = function(infill.crit, model, control, par.set, opt.path) {
  newdesign = generateDesign(control$seq.design.points, par.set, 
    randomLHS, ints.as.num=TRUE)
  y = infill.crit(newdesign, model, control, par.set, opt.path)
  newdesign[rank(y, ties.method="random") == 1, , drop=FALSE]
}

infillOptCMAES = function(infill.crit, model, control, par.set, opt.path) {
  # extract lower and upper bound for params
  low = getLower(par.set)
  upp = getUpper(par.set)
  
  rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  #FIXME: eval all point for one 1 gen at once?
  f = function(x) {
    newdata = as.data.frame(t(x))
    colnames(newdata) = rep.pids
    infill.crit(newdata, model, control, par.set, design)
  }
  # FIXME: handle restarts
  results = list()
  for (i in 1:2) {
    start = unlist(sampleValue(par.set))
    results[[i]] = cma_es(par=start, fn=f, lower=low, upper=upp, control=control$cmaes.control)
  }
  ys = extractSubList(results, "value")
  j = which(rank(ys, ties.method="random") == 1)
  as.data.frame(t(results[[j]]$par))
}

# FIXME: allow DiceOptim optimizer later...
# infillOptEI = function(infill.crit, model, control, par.set, opt.path) {
#   # extract lower and upper bound for params
#   low = getLower(par.set)
#   upp = getUpper(par.set)
#   
#   i = getOptPathBestIndex(opt.path, ties="random")
#   start = unlist(getOptPathEl(opt.path, i)$x)
#   capture.output(design <- max_EI(model$learner.model, 
#     lower=low, upper=upp, parinit=start)$par)
#   as.data.frame(design)
# }
