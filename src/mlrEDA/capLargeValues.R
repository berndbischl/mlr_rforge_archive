#' Converts Dates in a data.frame to day since start date(s).
#'
#' @param data [data.frame]\cr 
#'   Data to convert. Only \code{Date} columns will be changed.
#' @param threshold [numeric]\cr
#'   Threshold for capping in \code{impute.large}. Default is \code{Inf}. 
#' @param impute [numeric]\cr
#'   Value larger numeric entries then the treshold are replaced by. Default is \code{threshold}. 
#' 
#' @return [\code{data.frame}], where \code{Date} columns 
#'   have been tranformed to numeric days.
#' 
#' @export
#' @title Summarize factors of a data.frame.

capLargeValues = function(data, threshold, impute) {
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
    # large
    if (is.numeric(v)) {
      j = !is.na(v) & abs(v) > large 
      if (any(j)) {
        conv.large = c(conv.large, cn)
        v[j] = sign(v[j]) * impute.large  
        data[,i] = v
      }
    }
    
    # infs 
    j = is.infinite(v)
    if (any(j)) {
      conv.inf = c(conv.inf, cn)
      v[j] = sign(v[j]) * impute.inf  
      data[,i] = v
    }
    
    data[, x] = unclass(data[, x]) - unclass(start.date[x])
  }
  return(data)
}




