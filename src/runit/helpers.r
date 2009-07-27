library(reshape)

e1071.cv.to.mlr.cv <- function(e1071.tune.result) {
  tr <- e1071.tune.result
  inds <- tr$train.ind
  
  size <- max(melt.list(inds)$value)
  folds <- length(inds)

  cv.instance <- make.cv.instance(size=size, iters=folds)

  for (i in 1:folds)
    cv.instance@inds[[i]] = inds[[i]]
    
  return (cv.instance)
}


e1071.bs.to.mlr.bs <- function(e1071.tune.result) {
  tr <- e1071.tune.result
  inds <- tr$train.ind

  size <- length(inds[[1]])
  iters <- length(inds)
  
  bs.instance <- make.bs.instance(size=size, iters=iters)

  for (i in 1:iters)
	bs.instance@inds[[i]] = inds[[i]]
  
  return (bs.instance)
}