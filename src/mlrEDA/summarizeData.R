#' Get general characteristics of data.frame. 
#' 
#' @param data [\code{data.frame}]\cr 
#'   Data to summarize. 
#' @param target [\code{character(1)}]\cr 
#'   Target column. 
#' @param large [\code{numeric(1)}]\cr 
#'   What is a large (absolute) value for a numeric column? 
#' @param feat.perc [\code{logical(1)}]\cr 
#'   Portion of features in percent? Default is \code{FALSE}. 
#' @param na.perc [\code{logical(1)}]\cr 
#'   Portion of NAs in percent? Default is \code{FALSE}. 
#' @param class.perc [\code{logical(1)}]\cr 
#'   Portion of classes in percent? Default is \code{FALSE}. 
#' @param large.perc [\code{logical(1)}]\cr 
#'   Portion of large values in percent? Default is \code{FALSE}. 
#' @return [named \code{numeric}], containing characteristics.
#' @export

summarizeData = function(data, target, large=1e10,
  feat.perc=FALSE, na.perc=FALSE, class.perc=FALSE, large.perc=FALSE) {
  
  check.arg(target, "character", 1)
  check.arg(large, "numeric", 1)
  check.arg(feat.perc, "logical", 1)
  check.arg(na.perc, "logical", 1)
  check.arg(class.perc, "logical", 1)
  check.arg(large.perc, "logical", 1)
  
  x = numeric(0)
  
  NAs = sum(is.na(data))
  rows.with.missings = sum(apply(data, 1, function(x) any(is.na(x))))
  cols.with.missings = sum(apply(data, 2, function(x) any(is.na(x))))
  tt = data[, target]

  x["obs"] = nrow(data)
  x["dim"] = ncol(data)-1
  
  x["num"] = sum(sapply(data, is.numeric)) - is.numeric(tt)
  x["int"] = sum(sapply(data, is.integer))- is.integer(tt)
  x["fact"] = sum(sapply(data, is.factor)) - is.factor(tt)
  x["char"] = sum(sapply(data, is.character)) - is.character(tt)
  x["log"] = sum(sapply(data, is.logical)) - is.logical(tt)
  x["Date"] = sum(sapply(data, function(x) is(x, "Date")))
  if (feat.perc) {
    x["num"] = x["num"] / x["dim"]  
    x["int"] = x["int"] / x["dim"]  
    x["fact"] = x["fact"] / x["dim"]  
    x["char"] = x["char"] / x["dim"]  
    x["log"] = x["log"] / x["dim"]  
    x["Date"] = x["Date"] / x["dim"]  
  }
  
  x["na.row.max"] = max(Reduce(function(a,b) is.na(a) + is.na(b), data, init=0))
  x["na.col.max"] = max(sapply(data, function(y) sum(is.na(y))))
  if (na.perc) {
    x["na.row.max"] = x["na.row.max"] / x["dim"]  
    x["na.col.max"] = x["na.col.max"] / x["obs"]  
  }  
  
  g = function(y) if (is.numeric(y)) as.integer(y >= large) else 0 
  x["large.row.max"] = max(Reduce(function(a,b) a + g(b), data, init=0))
  x["large.col.max"] = max(sapply(data, function(y)
    if(is.numeric(y)) sum(abs(y) >= large) else 0))
  if (large.perc) {
    x["large.row.max"] = x["large.row.max"] / x["dim"]  
    x["large.col.max"] = x["large.col.max"] / x["obs"]  
  }  
  
  if (is.factor(tt) || is.character(tt)) {
    tab = table(tt)
    tab2 = prop.table(tab)
    x["classes"] = sum(tab)
    if (class.perc) {
      x["class.max"] = min(tab2)
      x["class.min"] = max(tab2)
    } else {
      x["class.max"] = min(tab)
      x["class.min"] = max(tab)
    }
    x["class.quot"] = x["class.max"] / x["class.min"]
  }
  
  return(x)
}
