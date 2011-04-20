#' Control structure for basic data preparation.
#'
#' @param ints.as [\code{character(1)}]\cr
#'   Should integer features be converted to either "numeric" or "factor". Default is "numeric".
#' @param chars.as [\code{character(1)}]\cr
#'   Conversion of character features. Currently only "factor" is supported.
#' @param logs.as [\code{character(1)}]\cr
#'   Should logical features be converted to either "numeric" or "factor". Default is "factor".
#' @param drop.class.levels [\code{logical(1)}]\cr
#'   Should empty class levels be dropped? Default is TRUE.
#' @return Control structure for data preparation.
#' @export
#' @usage prepare.control(ints.as, chars.as, logs.as, drop.class.levels = TRUE) 
#' @rdname prepare.control
#' @title Control for basic basic data preparation.



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


convertColumns = function(data, target, type, integers.as=as.numeric, characters.as=as.factor, logicals.as=as.factor) {
 
  mlr:::check.arg(integers.as, "function")  
  mlr:::check.arg(characters.as, "function")  
  mlr:::check.arg(logicals.as, "function")
  
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
