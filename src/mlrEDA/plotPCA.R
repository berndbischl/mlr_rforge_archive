#' Plots a PCA screeplot for the numeric columns of a data.frame.  
#'
#' @param data [\code{data.frame}]\cr 
#'   Data for PCA. Only numeric columns will be used and the target column excluded.
#' @param target [\code{character(1)}]\cr 
#'   Target column. 
#' @param exclude [\code{character}]
#'   Names of inputs, which should be excluded. Default is none.
#' @param scale [\code{logical(1)}]  
#'  Whether the variables should be scaled to have unit variance before the analysis takes place.
#'  The default is \code{TRUE} 
#' @return A data.frame with the columns: 'name', 'type', 'disp', 'mean', 'min', 'max', 'nlevs'.
#'   'disp' is a measure of dispersion, for numerics and integers \code{\link{sd}} is used, for 
#'   categorical columns the unstandardized index of qualitative variation M1 is computed. 'nlevs' is 
#'   the number of factor levels.
#' 
#' @export
#' @title Plots a PCA screeplot.

plotPCA = function(data, target, exclude=character(0), scale=TRUE, ...) {
  x = generatePCAResult(data, target, exclude, scale)
  plot(x, ...)
}


plot.PCAResult = function(x, ...) {
  screeplot(x, type="lines", ...)
}

