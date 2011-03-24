#' Plots a PCA screeplot for the numeric columns of a data.frame.  
#'
#' @param data [data.frame]\cr 
#'   Data for PCA. Only numeric columns will be used and the target column excluded. 
#'   Characters and logicals will be treated as factors.   
#' @return A data.frame with the columns: 'name', 'type', 'disp', 'mean', 'min', 'max', 'nlevs'.
#'   'disp' is a measure of dispersion, for numerics and integers \code{\link{sd}} is used, for 
#'   categorical columns the unstandardized index of qualitative variation M1 is computed. 'nlevs' is 
#'   the number of factor levels.
#' 
#' @export
#' @seealso \code{\link{makeVarselWrapper}} 
#' @title Variable selection.

plotPCA = function(data, target, exclude=character(0), scale=TRUE, ...) {
  x = generatePCAResult(data, target, exclude, scale)
  plot(x, ...)
}


plot.PCAResult = function(x, ...) {
  screeplot(x, type="lines", ...)
}

