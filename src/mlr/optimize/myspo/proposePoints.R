# returns list of points
#todo: minimize
proposePoints = function(model, par.set, control) {
  if (control$propose.points.method == "seq.design") {
    des = makeDesign(control$seq.design.points, par.set, control$seq.design.fun, control$seq.design.args)
    y = predict(model, newdata=des)@df$response
    o = order(y)
    points = des[o[1:control$propose.points],]
    return(lapply(1:nrow(points), function(i) as.list(points[i,])))
  }
}
