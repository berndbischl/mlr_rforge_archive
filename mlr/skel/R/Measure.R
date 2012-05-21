#' Construct performance measure.
#'
#' A measure object encapsulates a function to evaluate the performance of a prediction.
#' Information about already implemented measures can be obtained here: \code{\link{measures}}.
#' 
#' A learner is trained on a a training set d1, results in a model m, predicts another set d2 (which may be a different one
#' or the training set), resulting in the prediction. The performance measure can now be defined using all of the information of
#' the original task, the fitted model and the prediction.   
#' 
#' Object slots:
#' \describe{
#' \item{id [\code{character(1)}]}{See argument.}
#' \item{minimize [\code{logical(1)}]}{See argument.}
#' \item{classif [\code{logical(1)}]}{See argument.}
#' \item{regr [\code{logical(1)}]}{See argument.}
#' \item{only.binary [\code{logical(1)}]}{See argument.}
#' \item{allowed.pred.types [\code{character}]}{See argument.}
#' \item{req.pred [\code{logical(1)}]}{Is prediction object required in calculation?}
#' \item{req.task [\code{logical(1)}]}{Is task object required in calculation?.}
#' \item{req.model [\code{logical(1)}]}{Is model object required in calculation?}
#' \item{fun [\code{function}]}{See argument.}
#' \item{extra.args [\code{list}]}{See argument.}
#' \item{aggr [\code{\link{Aggregation}}]}{Associated aggregation function}.
#' }
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#' @param minimize [\code{logical(1)}]\cr
#'   Should the measure be minimized?
#'   Default is TRUE.
#' @param classif [\code{logical(1)}]\cr
#'   Is the measure applicable for classification?
#'   Default is FALSE.
#' @param regr [\code{logical(1)}]\cr
#'   Is the measure applicable for regression?
#'   Default is FALSE.
#' @param only.binary [\code{logical(1)}]\cr
#'   Is the measure only applicable to binary classification?
#'   Only reasonable if \code{classif} is \code{TRUE}.
#'   Default is \code{FALSE}.
#' @param allowed.pred.types [\code{character}]\cr
#'   Which prediction types are allowed for this measure?
#'   Subset of \dQuote{response},\dQuote{prob}, \dQuote{se}.
#'   Default is \code{character(0)}.
#' @param fun [\code{function(task, model, pred, extra.args)}]\cr
#'   Calculates performance value.
#' @param extra.args [\code{list}]\cr
#'   List of extra arguments which will always be passed to \code{fun}.      
#'   Default is empty list.
#' @return [\code{\link{Measure}}].
#' @export
#' @examples
#' f <- function(task, model, pred, extra.args) 
#'   sum((pred$data$response - pred$data$truth)^2)
#' makeMeasure(id="my.sse", minimize=TRUE, regr=TRUE, allowed.pred.types="response", fun=f)
makeMeasure = function(id, minimize, classif=FALSE, regr=FALSE,
  only.binary=FALSE, allowed.pred.types=character(0), fun, extra.args=list()) {

  checkArg(id, "character", len=1, na.ok=FALSE)
  checkArg(minimize, "logical", len=1, na.ok=FALSE)
  checkArg(classif, "logical", len=1, na.ok=FALSE)
  checkArg(regr, "logical", len=1, na.ok=FALSE)
  checkArg(only.binary, "logical", len=1, na.ok=FALSE)
  checkArg(allowed.pred.types, subset=c("response", "prob", "se"))
  checkArg(fun, "function")
  checkArg(extra.args, "list")

  fun1 = fun
  formals(fun1) = list()
  v = codetools:::findGlobals(fun1, merge=FALSE)$variables
  if (only.binary && !classif)
    stop("only.binary can only be set to TRUE, if 'classif' is set to TRUE!")

  m = structure(list(
    id=id, 
    minimize=minimize, 
    classif=classif, 
    regr=regr, 
    only.binary=only.binary,
    allowed.pred.types=allowed.pred.types, 
    req.pred="pred" %in% v, 
    req.model="model" %in% v, 
    req.task="task" %in% v,
    fun=fun, 
    extra.args=extra.args
  ), class="Measure")
  setAggregation(m, test.mean)
}

print.Measure = function(x, ...) {
  catf("Performance measure: %s", object$id)
  catf("Minimize: %s", object$minimize)
  catf("Aggregated by: %s", object$aggr$id)
}

default.measures = function(x) {
  if (is(x, "SupervisedTask")) {
    if (x$task.desc$type == "classif")
      return(list(mmce))
    else if (x$task.desc$type == "regr")
      return(list(mse))
    else
      stop("Should not happen!")
  }
  if (is(x, "Learner")) {
    if (x$type == "classif")
      return(list(mmce))
    else if (x$type == "regr")
      return(list(mse))
    else
      stop("Should not happen!")
  }
} 


#' Set aggregation function of measure. 
#'
#' Set how this measure will be aggregated after resampling. 
#' To see possible aggregation functions: \code{\link{aggregations}}.
#' 
#' @param measure [\code{\link{Measure}}]\cr 
#'   Performance measure.   
#' @param aggr [\code{\link{Aggregation}}]\cr
#'   Aggregation function.
#' @return [\code{\link{Measure}}] with changed aggregation behaviour.
setAggregation = function(measure, aggr) {
  checkArg(measure, "Measure")
  checkArg(aggr, "Aggregation")
  measure$aggr = aggr
  return(measure)
} 



