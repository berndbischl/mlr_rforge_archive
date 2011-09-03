#todo: do something smart if factors are there. maybe the same when propose.points?
#todo: dont use fix nr for design!
chooseFinalPoint = function(fun, par.set, model, opt.path, y.name, control) {
  df = as.data.frame(opt.path)
  input.names = setdiff(colnames(df), c(y.name, "dob", "eol"))
  if (control@final.point == "last.proposed") {
    i = nrow(df)
  } else if (control@final.point == "best.true.y") {
    i = getBestIndex(opt.path, ties="random")
  } else if(control@final.point == "best.predicted") {
    y = predict(model, newdata=df[, input.names])@df$response
    i = sample(which(min(y) == y), 1)
  }
  return(i)
}