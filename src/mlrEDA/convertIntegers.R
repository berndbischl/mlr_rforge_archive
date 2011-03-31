#' Converts dates in a data.frame.
#'
#' @param data [\code{data.frame}]\cr 
#'   Data to convert. 
#' @param which [\code{data.frame}]\cr
#'   Restrict result to columns in \code{which}. 
#'   Default is all columns of \code{data}.   
#' @param as [\code{function}]\cr
#'   Function used for conversion. 
#'   Default is \code{as.numeric} for Integers, \code{as.factor} for Characters and \code{as.factor} for Logicals. \cr
#'  
#' @return [\code{data.frame}], where \code{Date} columns 
#'   have been tranformed to numeric days.
#' 
#' @export
#' @title Summarize factors of a data.frame.


convertColumns = function(data, ints.as, chars.as, logicals.as) {
 
  if(missing(ints.as)) ints.as=as.numeric
  if(missing(chars.as)) chars.as=as.factor
  if(missing(logicals.as)) logicals.as=as.factor
  
  cols_Int =  names(which(sapply(data, function(x) is(x, "integer"))))
  cols_Char = names(which(sapply(data, function(x) is(x, "character"))))
  cols_Log = names(which(sapply(data, function(x) is(x, "logical"))))
  

  
  for (x in cols_Int) {
    data[, x] = ints.as(data[,x])
  }
  
  for (x in cols_Char) {
	  data[, x] = chars.as(data[,x])
  }
  for (x in cols_Log) {
	  data[, x] = logicals.as(data[,x])
  }
  
  return(data)
}
