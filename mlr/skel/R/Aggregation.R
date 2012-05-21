#' A aggregation method reduces the performance values of the test (and possibly the training sets) to a single
#' value. To see all possible, implemented aggregations look at \code{\link{aggregations}}.  
#' 
#' The aggregation can access all relevant information of the result after resampling and combine them into 
#' a single value. Though usually something very simple like taking the mean of the test set performances is done.
#' 
#' Object slots:
#' \describe{
#' \item{id [\code{character(1)}]}{Name of aggregation method.}
#' \item{fun [\code{function(task, perf.test, perf.train, measure, group, pred)}]}{Aggregation function.}
#' }
NULL

makeAggregation = function(id, fun) {
  structure(list(id=id, fun=fun), class="Aggregation")
}
