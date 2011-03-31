generateMDSResult = function(data, target, exclude=character(0), metric) {
  if (target %in% exclude)
    stop("Cannot exclude target!")
  nok = colnames(data) %in% exclude 
  if (ncol(data) - sum(nok) < 3)
    stop("There should be at least 2 features for MDS!") 
  data = data[, !nok]
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

