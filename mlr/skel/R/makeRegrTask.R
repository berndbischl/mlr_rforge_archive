#' @export
#' @rdname SupervisedTask
makeRegrTask = function(id, data, target, exclude=character(0), blocking=factor(c()), check.data=TRUE) {
  task = makeSupervisedTask(id, data, target, exclude, blocking, check.data)
  task$desc = makeTaskDesc(data, target, "regr", id, length(blocking) > 0, as.character(NA))      
  class(task) = c("RegrTask", class(task))
  return(task)
}
