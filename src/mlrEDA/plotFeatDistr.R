#' Plots univariate distribution of a feature.
#'
#' @param data [\code{data.frame}]\cr 
#'   Data. 
#' @param target [\code{character(1)}]\cr 
#'   Target column. 
#' @param col [\code{character(1)} | \code{integer(1)}]\cr 
#'   Selected feature from \code{data}. 
#' @return A ggplot2 object. Print it to plot it.
#' @export
#' @title Plots univariate distribution of a feature.


plotFeatDistr = function(data, target, col) {
  if (is.numeric(col))
    col = colnames(data)[col]
  checkArg(data, "data.frame")
  checkArg(target, "character", 1)
  
  x = data[, col]
  if (is.numeric(x) | is.integer(x)) {
    a = aes_string(x=col, colour=target)
    ggplot(data, a) + 
      geom_density(size=3) + 
      geom_histogram(aes(y = ..density..), alpha=0.5)
  } else if (is.factor(x) | is.logical(x) | is.character(x)){
    a = aes_string(x=col)
    ggplot(data, a) +
      geom_histogram(aes(y=..density..)) +
      facet_wrap(as.formula(paste("~", target))) 
  } else {
    stop("Unsupported column class: ", class(x))
  }
}
