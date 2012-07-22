#' @S3method makeWrappedModel OptModel
makeWrappedModel.OptModel = function(learner, model, task.desc, subset, vars, time) {
  or = attr(model, "opt.result")
  attr(model, "opt.result") = NULL
  x = makeWrappedModel(learner, model, task.desc, subset, vars, time)
  x$opt.result = opt.result
  class(x) = c("OptModel", class(x))
  return(x)
}

#' @S3method print OptModel
print.OptModel = function(x, ...) {
  print.WrappedModel(object)
  cat("\nOptimzation result:\n")
  print(x$opt.result)
}
