#' @include LearnTask.R
roxygen()

#' General description object for a classification task.
#' Use \code{\link{make.task}} to create it.   
#' 
#' @exportClass ClassifTask
#' @title Classification task.
#' @seealso \code{\link{make.task}}


setClass(
  "MultiLabelTask",
  contains = c("LearnTask")
)



#' Constructor.
#' @title ClassifTask constructor

setMethod(
  f = "initialize",
  signature = signature("ClassifTask"),
  def = function(.Object, id, target, data, weights, blocking, control=control) {
    if (missing(data))
      return(make.empty(.Object))
    
    td = new("TaskDesc", data, target, "ClassifTask", id, 
      length(weights) > 0, length(blocking) > 0, as.character(NA))      
    
    # init positive
    levs = td@class.levels
    
    callNextMethod(.Object, data=data, weights=weights, blocking=blocking, control=control, task.desc=td)
  }
)


