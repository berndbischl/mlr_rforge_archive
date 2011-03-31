#' Standard conversion for columns of a data.frame.
#'
#' @param data [\code{data.frame}]\cr 
#'   Data to convert.
#' @param integers.as [\code{function}]\cr
#'   Function used for conversion. 
#'   Default is \code{as.numeric}
#' @param characters.as [\code{function}]\cr
#'   Function used for conversion. 
#'   Default is \code{as.factor}
#' @param logicals.as [\code{function}]\cr
#'   Function used for conversion. 
#'   Default is \code{as.factor}
#' @return [\code{data.frame}] 
#' 
#' @export
#' @title Standard conversion for columns of a data.frame.


convertColumns = function(data, integers.as=as.numeric, characters.as=as.factor, logicals.as=as.factor) {
 
  check.arg(integers.as, "function")  
  check.arg(characters.as, "function")  
  check.arg(logicals.as, "function")
  
  cols.ints =  which(sapply(data, function(x) is(x, "integer")))
  cols.char = which(sapply(data, function(x) is(x, "character")))
  cols.logs = which(sapply(data, function(x) is(x, "logical")))
  
  for (x in cols.ints) {
    data[, x] = integers.as(data[,x])
  }
  
  for (x in cols.char) {
	  data[, x] = characters.as(data[,x])
  }
  for (x in cols.logs) {
	  data[, x] = logicals.as(data[,x])
  }
  
  return(data)
}
