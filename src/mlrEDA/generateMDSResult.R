generateMDSResult = function(data, target, exclude=character(0), metric) {
  j = which(colnames(data) == target)
  if (missing(metric))
    d = daisy(data[, -j])
  else
    d = daisy(data[, -j], metric=metric)
  s = as.data.frame(cmdscale(d))
  colnames(s) = c("mds.x1", "mds.x2")
  s[, target] = data[, target]
  class(s) = c("MDSResult", class(s))
  return(s)
}

