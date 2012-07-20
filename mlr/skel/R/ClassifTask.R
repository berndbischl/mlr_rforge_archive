#' @export
#' @rdname SupervisedTask
makeClassifTask = function(id, data, target, weights, blocking, positive, check.data=TRUE) {
  task = makeSupervisedTask("classif", id, data, target, weights, blocking, positive, check.data)
  class(task) = c("ClassifTask", class(task))
  return(task)
}

#' @S3method print ClassifTask
print.ClassifTask = function(x, ...) {
  di = printToChar(table(getTaskTargets(x)))
  # remove 1st newline
  di = substr(di, 2, nchar(di))
  m = length(x$task.desc$class.levels)
  print.SupervisedTask(x)
  cat(
    "  Classes: ", m, "\n",
    paste("  ", di, sep=""), "\n",
    "  Positive class: ", x$task.desc$positive, "\n",
    sep = ""
  )
}
