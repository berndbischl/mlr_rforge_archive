# returns list of points
#FIXME: minimize
#FIXME: finalize cmaes + vectorized CMAES!!!
#FIXME: round ints for cmaes + EI
# FIXME: use CL when more than 1 point in EI 
# FIXME: use other fillin fromn DiceOptim 
proposePoints = function(model, par.set, control, opt.path) {
  lm = model$learner.model 
  low = getLower(par.set)
  upp = getUpper(par.set)
  if (control$propose.points.method == "seq.design") {
    des = generateDesign(control$seq.design.points, par.set, 
      control$seq.design.fun, control$seq.design.args, ints.as.num=TRUE)
    y = predict(model, newdata=des)$data$response
    o = order(y)
    des[o[1:control$propose.points],,drop=FALSE]
  } else if (control$propose.points.method == "CMAES") {
    rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
    f = function(x) {
      nd = as.data.frame(t(x))
      colnames(nd) = rep.pids
      predict(model, newdata=nd)$data$response
    }
    i = getOptPathBestIndex(opt.path, ties="random")
    start = unlist(getOptPathEl(opt.path, i)$x)
    des = cma_es(par=start, fn=f, lower=low, upper=upp, control=list(maxit=2))$par
    des = as.data.frame(t(des))
  } else if (control$propose.points.method == "EI") {
    i = getOptPathBestIndex(opt.path, ties="random")
    start = unlist(getOptPathEl(opt.path, i)$x)
    capture.output(des <- max_EI(model$learner.model, low, upp, parinit=start)$par)
    as.data.frame(des)
  }
}
