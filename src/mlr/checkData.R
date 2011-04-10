# column names
# missings

#' Checks the following things for a data.frame
#' - error-proof column names
#' - No missing values in target
#' - No empty factor levels
#' - No infinite values
#' - No NANs

checkData = function(data, target) {
  cns = colnames(data)
  y = data[, target]
  
  if (any(is.na(y)))
    stop("Target contains missing values!")
  if (is.factor(y) && any(table(y) == 0)) 
    stop("Target contains empty class levels!")
  
  for (i in 1:ncol(data)) {
    x = data[, i]
    cn = cns[i]
    if(!deparse(as.name(cn), backtick=TRUE) == cn)
      stop("Column name contains special characters: ", cn)
    if (is.numeric(x)) {
      if (any(is.infinite(x)))
        stop("Data contains infinite values in: ", cn)
      if (any(is.nan(x)))
        stop("Data contains NaN values in: ", cn)
    } else if (is.factor(x)) {
      if(any(table(x) == 0)) 
        stop("Data contains contains empty factor levels in: ", cn)
    } else {
      stop("Unsupported feature type in: ", cn, ", ", class(x))
    }
  }
}