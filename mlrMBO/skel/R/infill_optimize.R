# Determine infill points with sequential design.
# 
# @param infill.crit [\code{function}]\cr 
#   Infill criterion function.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model used for prediction.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object for mbo.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param opt.path [\code{\link[ParamHelpers]{OptPath}}]\cr
#   Optimization path to save of type \code{\link[ParamHelpers]{OptPath}}.
# @return New infill points.
infillOptDesign = function(infill.crit, model, control, par.set, opt.path) {
  design = generateDesign(control$seq.design.points, par.set, 
    control$seq.design.fun, control$seq.design.args, ints.as.num=TRUE)
  y = infill.crit(design, model)
  o = order(y)
  # best 'propose.points' are selected.    
  design[o[1:control$propose.points],,drop=FALSE]
}

infillOptCMAES = function(infill.crit, model, control, par.set, opt.path) {
  # extract lower and upper bound for params
  low = getLower(par.set)
  upp = getUpper(par.set)
  
  rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  f = function(x) {
    newdata = as.data.frame(t(x))
    colnames(newdata) = rep.pids
    infill.crit(newdata, model)
  }
  
  i = getOptPathBestIndex(opt.path, ties="random")
  start = unlist(getOptPathEl(opt.path, i)$x)
  des = cma_es(par=start, fn=f, lower=low, upper=upp, control=list(maxit=2))$par
  des = as.data.frame(t(des))
}

infillOptEI = function(infill.crit, model, control, par.set, opt.path) {
  # extract lower and upper bound for params
  low = getLower(par.set)
  upp = getUpper(par.set)
  
  i = getOptPathBestIndex(opt.path, ties="random")
  start = unlist(getOptPathEl(opt.path, i)$x)
  capture.output(design <- max_EI(model$learner.model, 
    lower=low, upper=upp, parinit=start)$par)
  as.data.frame(design)
}
