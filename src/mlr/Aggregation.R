#' A aggregation method reduces the performance values of the test (and possibly the training sets) to a single
#' value. To see all possible, implemented aggregations look at \code{\link{aggregations}}.  
#' 
#' The aggregation can access all relevant information of the result after resampling and combine them into 
#' a single value. Though usually something very simple like taking the mean of the test set performances is done.
#' 
#' @slot id Name of aggregation function.
#' @slot fun Aggregation function: \code{function(perf.test, perf.train, measure, group, pred))}, where data types of arguments are: \code{numeric}, \code{numeric}, \code{\linkS4class{Measure}}, \code{factor}, \code{\linkS4class{ResamplePrediction}}. 
#'  
#' @exportClass Aggregation
#' @seealso \code{\link{aggregations}}, \code{\linkS4class{Measure}}, \code{\link{measures}} 

setClass(
  "Aggregation",
  representation = representation(
    id = "character",
    fun = "function"
  )
)


