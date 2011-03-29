#' Converts integers in a data.frame.
#'
#' @param data [data.frame]\cr 
#'   Data to convert. Only \code{integer} columns will be changed.
#' @param which [\code{data.frame}]\cr
#'   Restrict result to columns in \code{which}. 
#'   Default is all integer columns of \code{data}.   
#' @param as [\code{function}]\cr
#'   Function used for conversion. 
#'   Default is \code{as.numeric}.   
#'  
#' @return [\code{data.frame}], where \code{Date} columns 
#'   have been tranformed to numeric days.
#' 
#' @export
#' @title Summarize factors of a data.frame.

convertColumns = function(data, which, as) {
  n = ncol(data)
  cols =  names(which(sapply(data, function(x) is(x, "integer"))))
  
  for (x in date.names) {
    data[, x] = unclass(data[, x]) - unclass(start.date[x])
  }
  return(data)
}
