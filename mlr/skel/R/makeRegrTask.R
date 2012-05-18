#' @export
#' @rdname SupervisedTask
makeRegrTask = function(id, data, target, exclude, blocking, check.data=TRUE) {
  task = makeSupervisedTask("regr", id, data, target, exclude, blocking, positive, check.data)
  class(task) = c("RegrTask", class(task))
  return(task)
}
