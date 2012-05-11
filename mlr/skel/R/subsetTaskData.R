#' Subset data in task. 
#' 
#' @param x [\code{\linkS4class{LearnTask}}]\cr 
#'   Learning task.   
#' @param subset [\code{integer}] \cr 
#'   Selected cases. Default is all cases. 
#' @param vars [character] \cr 
#'   Selected inputs. Note that target feature is always included! Default is all input variables. 
#' @return \code{\linkS4class{LearnTask}} with changed data.
#' @exportMethod subsetData
#' @rdname subsetData
#' @seealso \code{\link{getData}} 
#' @title Subset data in task.

setGeneric(
  name = "subsetData",
  def = function(task, subset, vars) {
    if (missing(subset))
      subset = 1:task@desc@size
    if (missing(vars))
      vars = getFeatureNames(task)
    if (is.numeric(subset))
      subset = as.integer(subset)
    standardGeneric("subsetData")
  }
)

#' @rdname subsetData
setMethod(
  f = "subsetData",
  signature = signature(task="LearnTask", subset="integer", vars="character"),
  def = function(task, subset, vars) {
    task = changeData(task, getData(task, subset, vars))
    if (!missing(subset)) {
      if (task@desc@has.blocking)
        task@blocking = task@blocking[subset]
      if (task@desc@has.weights)
        task@weights = task@weights[subset]
    }  
    return(task)
  }
)
