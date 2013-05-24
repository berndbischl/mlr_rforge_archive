# Propose infill points.
#
# @param model [\code{\link{WrappedModel}}]\cr
#   Model used for prediction.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object for mbo.
# @param opt.path [\code{\link[ParamHelpers]{OptPath}}]\cr
#   Optimization path to save of type \code{\link[ParamHelpers]{OptPath}}.
# @return [\code{data.frame}] 
#   New infill points.
proposePoints = function(model, par.set, control, opt.path) {
  # generate a few random points if model failed
  if (inherits(model, "FailureModel"))
    return(generateDesign(control$propose.points, par.set, randomLHS, ints.as.num=TRUE))
  
  # determine infill criterion
  infill.crit.fun = switch(control$infill.crit,
    mean = infillCritMeanResponse,
    ei = infillCritEI,
    aei = infillCritAEI
  )
  
  # determine infill optimization strategy
  infill.opt = switch(control$infill.opt,
    design = infillOptDesign,
    cmaes = infillOptCMAES
    #EI       = infillOptEI
  )
  design = as.data.frame(opt.path)
  infill.opt(infill.crit.fun, model, control, par.set, design)
}

# # returns list of points
# #FIXME: minimize
# #FIXME: finalize cmaes + vectorized CMAES!!!
# #FIXME: round ints for cmaes + EI
# # FIXME: use CL when more than 1 point in EI 
# # FIXME: use other fillin fromn DiceOptim 
# proposePoints = function(model, par.set, control, opt.path) {
#   lm = model$learner.model 
#   # FIXME: returning a dataframe seens stupid (maybe not?), do interface
#   low = getLower(par.set)
#   upp = getUpper(par.set)
#   # FIXME: doc and think about it
#   # if we could not train the model, sample points
#   if (inherits(model, "FailureModel")) {
#     generateDesign(control$propose.points, par.set, randomLHS, ints.as.num=TRUE)    
#   } else if (control$propose.points.method == "seq.design") {
#     des = generateDesign(control$seq.design.points, par.set, 
#       control$seq.design.fun, control$seq.design.args, ints.as.num=TRUE)
#     y = predict(model, newdata=des)$data$response
#     o = order(y)
#     des[o[1:control$propose.points],,drop=FALSE]
#   } else if (control$propose.points.method == "CMAES") {
#     rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
#     f = function(x) {
#       nd = as.data.frame(t(x))
#       colnames(nd) = rep.pids
#       predict(model, newdata=nd)$data$response
#     }
#     i = getOptPathBestIndex(opt.path, ties="random")
#     start = unlist(getOptPathEl(opt.path, i)$x)
#     des = cma_es(par=start, fn=f, lower=low, upper=upp, control=list(maxit=2))$par
#     des = as.data.frame(t(des))
#   } else if (control$propose.points.method == "EI") {
#     i = getOptPathBestIndex(opt.path, ties="random")
#     start = unlist(getOptPathEl(opt.path, i)$x)
#     capture.output(des <- max_EI(model$learner.model, 
#       lower=low, upper=upp, parinit=start)$par)
#     as.data.frame(des)
#   }
# }