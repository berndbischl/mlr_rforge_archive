#' Summarizes a data.frame, somewhat differently than the normal summary function of R.
#'
#' @param data [data.frame]\cr 
#'   Data to summarize. Columns can be of type numeric, integer, logical, factor, character or Date. 
#'   Characters and logicals will be treated as factors.   
#' @param start.date [named vector of \code{Date} | single \code{Date}]\cr 
#'   Dates are converted to days passed since a start date. If this argument is missing,
#'   the start date is the mininal date of the feature. Either pass a single Date (which will be
#'   replicated for all Date features) or a named vector of date, which contains an element for every
#'   Date feature in \code{data}.
#'   Per default is \code{start.date} is missing.    
#' @return A data.frame with the columns: 'name', 'type', 'na', 'disp', 'mean', 'min', 'max', 'nlevs'.
#'   'disp' is a measure of dispersion, for numerics and integers \code{\link{sd}} is used, for 
#'   categorical columns the unstandardized index of qualitative variation M1 is computed. 
#'   'min' and 'max' for factors are the sizes of the smallest / largest category.
#'   'nlevs' is the number of factor levels.
#' 
#' @export
#' @title Summarize a data.frame.

summarizeData = function(data, start.date) {
  iqv = function(x) {
    #todo: remove empty levels
    k = length(levels)
    1 - sum(prop.table(table(x))^2)
  }
  n = ncol(data)
  cns = colnames(data)
  res = data.frame(name=character(n), type=character(n), na=integer(n), 
    disp=numeric(n), mean=numeric(n), median=numeric(n), 
    min=numeric(n), max=numeric(n), nlevs=integer(n), stringsAsFactors=FALSE)
  date.names =  names(which(sapply(data, function(x) is(x, "Date"))))
  
  if (missing(start.date)) {
    start.date = na.omit(sapply(data, function(x) 
          if(is(x, "Date")) min(unclass(x), na.rm=TRUE) else NA))
  } else {
    m = length(start.date)
    stopifnot(is(start.date, "Date") && (m == 1 || m == length(date.names)))
    if (m == 1) 
      start.date = unlclass(rep(start.date, length(date.names)))
  }
  
  for (i in 1:n) {
    x = data[,i]
    res[i, "na"] = sum(is.na(x))
    x = na.omit(x)
    res[i, "name"] = cns[i]
    res[i, "type"] = class(x)[1]
    if (is.numeric(x) | is.integer(x)) {
      res[i, "disp"] = sd(x) 
      res[i, "mean"] = mean(x) 
      res[i, "median"] = median(x) 
      res[i, "min"] = min(x) 
      res[i, "max"] = max(x) 
      res[i, "nlevs"] = as.integer(NA) 
    }else if (is(x, "Date")) {
      x = unclass(x) - start.date[cns[i]]
      res[i, "disp"] = sd(x) 
      res[i, "mean"] = mean(x) 
      res[i, "median"] = median(x) 
      res[i, "min"] = min(x) 
      res[i, "max"] = max(x) 
      res[i, "nlevs"] = as.integer(NA) 
    } else if (is.factor(x) | is.logical(x) | is.character(x)) {
      x = as.factor(x)
      tab = table(x)
      res[i, "disp"] = iqv(x)
      res[i, "mean"] = NA 
      res[i, "median"] = NA 
      res[i, "min"] = min(tab) 
      res[i, "max"] = max(tab) 
      res[i, "nlevs"] = length(levels(x))  
    } else {
      stop("Unsupported column class: ", class(x))
    }
  }
  return(res)
}
