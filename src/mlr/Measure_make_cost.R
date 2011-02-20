#' @include ClassifTask.R
roxygen()

#' Creates a measure for non-standard misclassification costs.
#' 
#' @param id [character(1)] \cr
#'   Name of measure. Default is "costs".
#' @param minimize [boolean] \cr
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
  name = "makeCostMeasure",
  def = function(id, minimize, costs, task, mean.costs) {
    if (missing(id))
      id = "costs"
    if (missing(minimize))
      minimize = TRUE
    if (missing(mean.costs))
      mean.costs = TRUE
    check.arg(mean.costs, "logical", 1)
    standardGeneric("makeCostMeasure")
  }
)


setMethod(
  f = "makeCostMeasure",
  signature = signature(id="character", minimize="logical", costs="matrix", task="ClassifTask"),
  def = function(id="costs", minimize=TRUE, costs, task, mean.costs) {
    check.costs(costs, task["class.levels"]) 
    makeMeasure(id="costs", minimize=minimize, extra.pars=list(costs, mean.costs), 
      fun=function(task, model, pred, extra.pars) {
        costs = extra.pars[[1]]
        mean.costs = extra.pars[[2]]
        # cannot index with NA
        r = pred@df$response    
        if (any(is.na(r)))
          return(as.numeric(NA))
        cc = function(truth, pred) {
          costs[truth, pred]
        }
        y = mapply(cc, as.character(pred@df$truth), as.character(r))
        if (mean.costs) mean(y) else sum(y)
      }
    )
  }
)
