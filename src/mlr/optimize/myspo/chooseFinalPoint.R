#todo: do something smart if factors are there. maybe the same when propose.points?
#todo: dont use fix nr for design!
chooseFinalPoint = function(fun, par.set, model, opt.path, y.name, control) {
  if (control@final.point == "best.in.path") {
    df = as.data.frame(opt.path)
    y = df[,y.name]
    df = df[, setdiff(colnames(df), c(y.name, "dob", "eol")), drop=FALSE]
    df[which.min(y),,drop=FALSE]
  } else if(control@final.point == "opt.pred") {
    des = makeDesign(100000, par.set, randomLHS, ints.as.num=TRUE)
    y = predict(model, newdata=des)@df$response
    des[which.min(y),,drop=FALSE]
  }
}