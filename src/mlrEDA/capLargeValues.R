#' Convert large/infinite numeric values in a data.frame.
#' Only numeric columns are affected.
#' 
#' @param data [data.frame]\cr 
#'   Data to convert. 
#' @param which [\code{character} | \code{integer}] 
#'   Which columns to convert. Default is all numeric columns
#' @param threshold [\code{numeric(1)}]\cr
#'   Threshold for capping. Every entry whose absolute value is equal or larger is converted.
#'   Default is \code{Inf}. 
#' @param impute [\code{numeric(1)}]\cr
#'   Replacement value for large entries. Default is \code{threshold}. Large negative entries are converted to 
#'  \code{ -threshold}.  
#' 
#' @return [\code{data.frame}]
#' 
#' @export
#' @title Convert large/infinite numeric values in a data.frame.

capLargeValues = function(data, threshold=Inf, impute=threshold) {
  check.arg(threshold, "numeric", 1)
  check.arg(impute, "numeric", 1)
  
  if (missing(which))
    which =  which(sapply(data, function(x) is.numeric(x)))
  
  for (x in which) {
    if (any(abs(data[, x]) >= threshold)) {
      data[, i] = pmin(pmax(fs[[i]], -threshold), threshold)
    }
  }
  return(data)
}




