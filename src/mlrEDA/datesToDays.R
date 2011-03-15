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
#' @return [\code{data.frame}], where \code{Date} columns 
#'   have been tranformed to numeric days.
#' 
#' @export
#' @title Summarize factors of a data.frame.

datesToDays = function(data, start.date) {
  n = ncol(data)
  date.names =  names(which(sapply(data, function(x) is(x, "Date"))))
  
  if (missing(start.date)) {
    start.date = na.omit(sapply(data, function(x) 
          if(is(x, "Date")) min(unclass(x), na.rm=TRUE) else NA))
  } else {
    if (is.character(start.date))
      start.date = as.Date(start.date)
    if (is(start.date, "Date") && length(start.date) == 1) {
      start.date = rep(start.date, length(date.names))
      names(start.date) = date.names
    }  
    if (!(is(start.date, "Date") && length(start.date) == length(date.names)))
      stop("start.date has wrong type or length!")
  }
  
  for (x in date.names) {
    data[, x] = unclass(data[, x]) - unclass(start.date[x])
  }
  return(data)
}
