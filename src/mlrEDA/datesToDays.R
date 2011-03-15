#' Converts Dates in a data.frame to day since start date(s).
#'
#' @param data [data.frame]\cr 
#'   Data to convert. Only \code{Date} columns will be changed.
#' @param start.date [named vector of \code{Date} | single \code{Date}]\cr 
#'   Dates are converted to days passed since a start date. If this argument is missing,
#'   the start date is the mininal date of the feature. Either pass a single Date (which will be
#'   replicated for all Date features) or a named vector of date, which contains an element for every
#'   Date feature in \code{data}.
#'   Per default is \code{start.date} is missing.    
#'  
#' @return [\code{data.frame}] with the columns 'na' (number of missing values),
#'   'min', 'max', 'span' (days between 'min' and 'max').
#' 
#' @export
#' @title Summarize factors of a data.frame.

datesToDays = function(data, start.date) {
  n = ncol(data)
  cns = colnames(data)
  date.names =  names(which(sapply(data, function(x) is(x, "Date"))))
  
  if (missing(start.date)) {
    start.date = na.omit(sapply(data, function(x) 
          if(is(x, "Date")) min(unclass(x), na.rm=TRUE) else NA))
  } else {
    m = length(start.date)
    stopifnot(is(start.date, "Date") && (m == 1 || m == length(date.names)))
    if (m == 1) 
      start.date = unclass(rep(start.date, length(date.names)))
  }
  
  for (x in date.names) {
    data[, x] = unclass(x) - start.date[cns[i]]
    res[i, "disp"] = sd(x) 
    res[i, "mean"] = mean(x) 
    res[i, "median"] = median(x) 
    res[i, "min"] = min(x) 
    res[i, "max"] = max(x) 
    res[i, "nlevs"] = as.integer(NA) 
  }
  return(data)
}
