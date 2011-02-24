# returns list of points
#todo: minimize
proposePoints = function(model, par.set, control) {
  lm = model["learner.model"] 
  if (control@propose.points.method == "seq.design") {
    des = makeDesign(control@seq.design.points, par.set, control@seq.design.fun, control@seq.design.args)
    y = predict(model, newdata=des)@df$response
    o = order(y)
    des[o[1:control@propose.points],]
  } else if (control@propose.points.method == "EI") {
    
    # todo: use CL when more than 1 point 
    # todo: handle ints 
    low = lower(par.set)
    upp = upper(par.set)
    #max_EI(model, lower, upper, parinit, control)$par
  }
}
