#' @export
#' @rdname SupervisedTask
makeRegrTask = function(id, data, target, weights, blocking, check.data=TRUE) {
  task = makeSupervisedTask("regr", id, data, target, weights, blocking, as.character(NA), check.data)
  class(task) = c("RegrTask", class(task))
  return(task)
}
