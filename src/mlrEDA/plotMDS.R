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
#' @seealso \code{\link{makeVarselWrapper}} 
#' @title Variable selection.

plotMDS = function(data, target, exclude=character(0), metric) {
	require(cluster)
	require(ggplot2)
  j = which(colnames(data) == target)
  if (missing(metric))
    d = daisy(data[, -j])
  else
    d = daisy(data[, -j], metric=metric)
  s = as.data.frame(cmdscale(d))
  colnames(s) = c("mds.x1", "mds.x2")
  s[, target] = data[, target]
  ggplot(s, aes_string(x="mds.x1", y="mds.x2", colour=target)) + geom_point()
  
}

