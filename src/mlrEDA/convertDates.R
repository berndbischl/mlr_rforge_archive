#' Converts Dates in a data.frame.
#' Each date is transformed to: numeric days since a start date, the month (factor) 
#' and the weekday (factor).
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
#' @return [\code{data.frame}]
#' 
#' @export
#' @title Converts Dates in a data.frame.

convertDates = function(data, days=TRUE, weekdays=TRUE, months=TRUE, start.date) {
  n = ncol(data)
  date.names =  names(which(sapply(data, function(x) is(x, "Date"))))
  
  if (missing(start.date)) {
    start.date = na.omit(sapply(data, function(x) 
          if(is(x, "Date")) min(unclass(x), na.rm=TRUE) else NA))
  } else {
    ns = names(start.date)
    if (is.character(start.date)) {
      start.date = as.Date(start.date)
      names(start.date) = ns
    }  
    if (is(start.date, "Date") && length(start.date) == 1) {
      start.date2 = rep(start.date, length(date.names))
      names(start.date) = date.names
    }  
    if (!(is(start.date, "Date") && length(start.date) == length(date.names)))
      stop("start.date has wrong type or length!")
    if (!setequal(names(start.date), date.names))    
      stop("start.date has wrong names!")
  }
  for (x in date.names) {
    if (days)
      data[, paste(x, "days", sep="_")] = unclass(data[, x]) - unclass(start.date[x])
    if (weekdays)
      data[, paste(x, "weekdays", sep="_")] = factor(weekdays(data[, x], abbreviate=TRUE))
    if (months)
      data[, paste(x, "months", sep="_")] = factor(months(data[, x], abbreviate=TRUE))
    data[, x] = NULL
  }
  return(data)
}
