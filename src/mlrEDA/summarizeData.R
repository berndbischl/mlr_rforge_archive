#' Summarizes a data.frame, somewhat differently than the normal summary function of R.
#'
#' @param data [data.frame]\cr 
#'   Data to summarize. Columns can be of type numeric, integer, logical, factor or character. 
#'   Characters and logicals will be treated as factors.   
#' @return A data.frame with the columns: 'name', 'type', 'disp', 'mean', 'min', 'max', 'nlevs'.
#'   'disp' is a measure of dispersion, for numerics and integers \code{\link{sd}} is used, for 
#'   categorical columns the unstandardized index of qualitative variation M1 is computed. 'nlevs' is 
#'   the number of factor levels.
#' 
#' @export
#' @title Summarize a data.frame.

summarizeData = function(data) {
  iqv = function(x) {
    #todo: remove empty levels
    k = length(levels)
    1 - sum(prop.table(table(x))^2)
  }
  n = ncol(data)
  cns = colnames(data)
  res = data.frame(name=character(n), type=character(n), disp=numeric(n), 
    mean=numeric(n), min=numeric(n), max=numeric(n), nlevs=integer(n), stringsAsFactors=FALSE)
  
  for (i in 1:n) {
    x = na.omit(data[,i])
    res[i, "name"] = cns[i]
    res[i, "type"] = class(x)
    if (is.numeric(x) | is.integer(x)) {
      res[i, "disp"] = sd(x) 
      res[i, "mean"] = mean(x) 
      res[i, "min"] = min(x) 
      res[i, "max"] = max(x) 
      res[i, "nlevs"] = as.integer(NA) 
    } else if (is.factor(x) | is.logical(x) | is.character(x)) {
      x = as.factor(x)
      res[i, "disp"] = iqv(x)
      res[i, "mean"] = NA 
      res[i, "min"] = NA 
      res[i, "max"] = NA 
      res[i, "nlevs"] = length(levels(x))  
    } else {
      stop("Unsupported column class: ", class(x))
    }
  }
  return(res)
}
