#' Plots the data by multi-dimensional scaling.
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
#' @title Multi-dimensional scaling.
plotMDS = function(data, target, exclude=character(0), metric) {
  x = generateMDSResult(data, target, exclude, metric)
  plot(x)
}

plot.MDSResult = function(x) {
  ggplot(x, aes_string(x="mds.x1", y="mds.x2", colour=colnames(x)[3])) + geom_point()
}

