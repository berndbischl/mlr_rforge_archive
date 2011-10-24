
#' @include ClassifTask.R
roxygen()

#' Creates a measure for non-standard misclassification costs.
#' 
#' @param id [\code{character(1)}] \cr
#'   Name of measure. Default is "costs".
#' @param minimize [\code{logical(1)}] \cr
#'   Should the measure be minimized? Default is TRUE. Otherwise you are effectively specifying a benefits matrix.
#' @param costs [Numerical matrix] \cr
#'   Matrix of misclassification costs. Rows and columns have to be named with class labels, order does not matter. 
#'   Rows indicate true classes, columns predicted classes.
#' @param task [\code{\linkS4class{ClassifTask}}]\cr 
#'   Classification task. Has to be passed, so validity of matrix names can be checked.
#' @param mean.costs [single logical] \cr
#'   Should costs be averaged (TRUE) or summed (FALSE) over all cases in one test set prediction? Default is TRUE.
#' 
#' @return \code{\linkS4class{Measure}} 
#' 
#' @exportMethod makeCostMeasure
#' @rdname makeCostMeasure
#' @seealso \code{\link{measures}}, \code{\link{makeMeasure}}
#' @title Create cost measure.


setGeneric(
  name = "makeVarCostMeasure",
  def = function(id, minimize, costs, task, mean.costs) {
    if (missing(id))
      id = "costs"
    if (missing(minimize))
      minimize = TRUE
    if (missing(mean.costs))
      mean.costs = TRUE
    check.arg(mean.costs, "logical", 1)
    standardGeneric("makeVarCostMeasure")
  }
)

#' @rdname makeCostMeasure
setMethod(
  f = "makeVarCostMeasure",
  signature = signature(id="character", costs="numeric", task="ClassifTask"),
  def = function(id="costs", minimize=TRUE, costs, task, cost.aggr) {
    check.costs(costs, task@desc@class.levels) 
    makeMeasure(id="costs", minimize=TRUE, classif=TRUE, regr=TRUE, extra.args=list(costs, cost.aggr), 
      fun=function(task, model, pred, extra.args) {
        costs = extra.args[[1]]
        cost.aggr = extra.args[[2]]
        mean.costs = extra.args[[2]]
        cost.aggr(model@vars * costs)
      }
    )
  }
)
