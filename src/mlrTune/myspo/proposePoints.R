# returns list of points
#todo: minimize
#todo: finalize cmaes + vectorized CMAES!!!
#todo: round ints for cmaes + EI
# todo: use CL when more than 1 point in EI 
proposePoints = function(model, par.set, control, opt.path) {
  lm = model@learner.model 
  low = lower(par.set)
  upp = upper(par.set)
  if (control@propose.points.method == "seq.design") {
    des = makeDesign(control@seq.design.points, par.set, 
      control@seq.design.fun, control@seq.design.args, ints.as.num=TRUE)
    y = predict(model, newdata=des)@df$response
    o = order(y)
    des[o[1:control@propose.points],,drop=FALSE]
  } else if (control@propose.points.method == "CMAES") {
    rep.pids = getRepeatedParameterIDs(par.set, with.nr=TRUE)
    f = function(x) {
      nd = as.data.frame(t(x))
      colnames(nd) = rep.pids
      predict(model, newdata=nd)@df$response
    }
    i = getBestIndex(opt.path, ties="random")
    start = unlist(getPathElement(opt.path, i)$x)
    des = cma_es(par=start, fn=f, lower=low, upper=upp, control=list(maxit=2))$par
    des = as.data.frame(t(des))
  } else if (control@propose.points.method == "EI") {
    i = getBestIndex(opt.path, ties="random")
    start = unlist(getPathElement(opt.path, i)$x)
    capture.output(des <- max_EI(model@learner.model, low, upp, parinit=start)$par)
    as.data.frame(des)
  }
}
