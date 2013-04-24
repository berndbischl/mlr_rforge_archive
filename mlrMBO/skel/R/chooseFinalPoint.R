#FIXME: do something smart if factors are there. maybe the same when propose.points?
#FIXME: dont use fix nr for design!
chooseFinalPoint = function(fun, par.set, model, opt.path, y.name, control) {
  df = as.data.frame(opt.path, strings.as.factors=TRUE)
  input.names = setdiff(colnames(df), c(y.name, "dob", "eol"))
  if (control$final.point == "last.proposed") {
    i = nrow(df)
  } else if (control$final.point == "best.true.y") {
    i = getOptPathBestIndex(opt.path, ties="random")
  } else if(control$final.point == "best.predicted") {
    y = predict(model, newdata=df[, input.names])$data$response
    # FIXME: do we really want to allow for NAs here if model breaks?
    i = which(rank(y, ties.method = "random") == 1)
  }
  return(i)
}