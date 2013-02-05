#' @export
#' @rdname SupervisedTask
makeClassifTask = function(id, data, target, blocking, positive, check.data=TRUE) {
  task = makeSupervisedTask("classif", id, data, target, blocking, positive, check.data)
  class(task) = c("ClassifTask", class(task))
  return(task)
}

#' @S3method print ClassifTask
print.ClassifTask = function(x, ...) {
  # remove 1st newline
  di = printToChar(table(getTaskTargets(x)), collapse=NULL)[-1]
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  catf("Classes: %i", m)
  cat(paste(di, "\n"))
  catf("Positive class: %s", x$task.desc$positive)
}
