#' @export
#' @rdname SupervisedTask
makeClassifTask = function(id, data, target, exclude, blocking, positive, check.data=TRUE) {
  task = makeSupervisedTask("classif", id, data, target, exclude, blocking, positive, check.data)
  class(task) = c("ClassifTask", class(task))
  return(task)
}
   
print.ClassifTask = function(x, ...) {
  di = printToChar(table(getTargets(x)))
  # remove 1st newline
  di = substr(di, 2, nchar(di))
  m = length(x$desc$class.levels)
  print.SupervisedTask(x)
  cat(
    "Classes: ", m, "\n",
    di, "\n",
    "Positive class: ", x$desc$positive, "\n",
    sep = ""
  )
}
