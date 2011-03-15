#' Summarizes dates of a data.frame by showing minimum and maximum dates and their span.
#'
#' @param data [data.frame]\cr 
#'   Data to summarize. Only \code{Date} columns will be considered.
#' @param which [\code{data.frame}]\cr
#'   Restrict resut to columns in \code{which}. 
#'   Default is all Date columns of \code{data}.   
#'  
#' @return [\code{data.frame}] with the columns 'na' (number of missing values),
#'   'min', 'max', 'span' (days between 'min' and 'max').
#' 
#' @export
#' @title Summarize factors of a data.frame.

summarizeDates = function(data, which) {
  n = ncol(data)
  cns = colnames(data)
  pred = function(x) is(x, "Date")    
  if (missing(which)) 
    which = Filter(function(x) pred(data[,x]), cns)
  else
  if (!all(which %in% cns)) 
    stop("Undefined columns selected!")
  
  res = data.frame(name=character(n), na=integer(n), 
    min=numeric(n), max=numeric(n), span=numeric(n))

  i = 1
  for (x in which) {
    if (!pred(data[,x]))
      stop(x, " is not a Date!")
    res[i, "name"] = x
    x = data[,x]
    res[i, "na"] = sum(is.na(x))
    x = na.omit(x)
    if (is(x, "Date")) {
      res[i, "min"] = sd(x) 
      res[i, "max"] = mean(x) 
      res[i, "span"] = max(x) - min(x) 
    } else {
      stop("Unsupported column class: ", class(x))
    }  
    i = i + 1
  }
  return(res)
}
