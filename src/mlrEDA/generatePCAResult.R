generatePCAResult = function(data, target, exclude=character(0), scale=TRUE) {
  nok = colnames(data) %in% c(target, exclude) | 
    sapply(data, function(x) !is.numeric(x))
  if (ncol(data) - sum(nok) < 3)
    stop("There should be at least 3 numeric columns for PCA!") 
  data = data[, !nok]
  x = prcomp(data, scale = TRUE)
  class(x) = c("PCAResult", class(x))
  return(x)
}

