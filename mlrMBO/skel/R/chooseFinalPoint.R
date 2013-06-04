chooseFinalPoint = function(fun, par.set, model, opt.path, y.name, control) {
  df = as.data.frame(opt.path, discretes.as.factor=TRUE)
  input.names = setdiff(colnames(df), c(y.name, "dob", "eol"))
  if (control$final.point == "last.proposed") {
    i = nrow(df)
  } else if (control$final.point == "best.true.y") {
    # works for maximize automatically
    i = getOptPathBestIndex(opt.path, ties="random")
  } else if(control$final.point == "best.predicted") {
    y = ifelse(control$minimize, 1, -1) * predict(model, newdata=df[, input.names])$data$response
    # FIXME: do we really want to allow for NAs here if model breaks?
    i = which(rank(y, ties.method = "random") == 1)
  }
  return(i)
}