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
  cns = colnames(data)
  pred = function(x) is(x, "Date")    
  if (missing(which)) 
    which = Filter(function(x) pred(data[,x]), cns)
  else
    if (!all(which %in% cns)) 
      stop("Undefined columns selected!")
  n = length(which)
  res = data.frame(name=character(n), na=integer(n), 
    min=numeric(n), max=numeric(n), span=numeric(n), stringsAsFactors=FALSE)
  class(res$min)  = "Date"
  class(res$max)  = "Date"
    
  for (i in seq_along(which)) {
    x = which[i]
    if (!pred(data[,x]))
      stop(x, " is not a Date!")
    res[i, "name"] = x
    x = data[,x]
    res[i, "na"] = sum(is.na(x))
    x = na.omit(x)
    res[i, "min"] = min(x) 
    res[i, "max"] = max(x) 
    res[i, "span"] = res[i, "max"] - res[i, "min"]
  }
  return(res)
}
