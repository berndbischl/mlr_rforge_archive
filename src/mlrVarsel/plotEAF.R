plotEAF = function(data, y.names, xlab=y.names[1], ylab=y.names[2], ...) {
  if (is.list(data)) {
    #todo: check that run is not present
    data = lapply(data, function(x) t(nondominated_points(t(as.matrix(x[, y.names])))))
    data = lapply(1:length(data), function(i) cbind(data[[i]], run=i))
    data = as.data.frame(do.call(rbind, data))
  }
  f = as.formula(sprintf("%s+%s~run", y.names[1], y.names[2]))
  eafplot(f, data=data, xlab=xlab, ylab=ylab, ...)
}