#' Construct your own performance measure, used after resampling. 
#' Note that individual training / test set performance values will be set to \code{NA}, you
#' only calculate an aggregated value. If you can define a function that makes sense
#' for every single training / test set, implement your own \code{\linkS4class{Measure}}.
#' 
#' @param id [\code{character(1)}] \cr
#'   Name of aggregated measure. 
#' @param minimize [\code{logical(1)}] \cr
#'   Should the measure be minimized? Default is \code{TRUE}. 
#' @param classif [\code{logical(1)}] \cr
#'   Is the measure applicable for classification? Default is \code{TRUE}.   
#' @param regr [\code{logical(1)}] \cr
#'   Is the measure applicable for regression? Default is \code{TRUE}.   
#' @param only.binary [\code{logical(1)}] \cr
#'   Is the measure only applicable to binary classification? 
#'   Only reasonable if \code{classif} is \code{TRUE}. 
#'   Default is \code{FALSE}. 
#' @param allowed.pred.types [\code{character}]
#'   Which prediction types are allowed for this measure? 
#'   Subset of \dQuote{response},\dQuote{prob}.
#'   Default is both.   
#' @param fun [\code{function(task, pred, group, pred, extra.args)}] \cr
#'   Calculates performance value from \code{\link{ResamplePrediction}} object. 
#'   For rare case you can also use the task, the grouping or the extra arguments \code{extra.args}. 
#' @param extra.args [\code{list}] \cr
#'   List of extra arguments which will always be passed to \code{fun}.      
#' 
#' @return \code{\linkS4class{Measure}} 
#' 
#' @export
#' @seealso \code{\link{Measures}}, \code{\link{measures}}, \code{\link{Aggregation}}, \code{\link{aggregation}} 
#' @title Construct your own resampled performance measure.

makeCustomResampledMeasure = function(id, minimize=TRUE, classif=TRUE, regr=TRUE, 
    only.binary=FALSE, allowed.pred.types=c("response", "prob"), fun, extra.args) {
    force(fun)
    fun1 = function(task, model, pred, extra.args) as.numeric(NA)
    custom = makeMeasure(id="custom", minimize, classif, regr, only.binary, allowed.pred.types, fun1, extra.args)
    fun2 = function(task, perf.test, perf.train, measure, group, pred) 
      fun(task, group, pred, extra.args)
    aggr = new("Aggregation", id=id, fun=fun2)
    setAggregation(custom, aggr)
}
