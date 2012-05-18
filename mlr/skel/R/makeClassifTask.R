#' @export
#' @rdname SupervisedTask
makeClassifTask = function(id, data, target, exclude=character(0), blocking=factor(c()), 
  positive=as.character(NA), check.data=TRUE) {
  
  task = makeSupervisedTask(id, data, target, exclude, blocking, check.data)
  levs = levels(task$env$data[,target])
  m = length(levs)
  if (is.na(positive)) {
    if (m <= 2)
      positive = levs[1]
  } else {
    if (m > 2)
      stop("Cannot set a positive class for a multiclass problem!")
    if (!(positive %in% levs))
      stop(paste("Trying to set a positive class", positive, "which is not a value of the target variable:", paste(levs, collapse=",")))
  } 
  task$desc = makeTaskDesc(data, target, "classif", id, length(blocking) > 0, positive)      
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
