library(reshape)

e1071.cv.to.mlr.cv <- function(e1071.tune.result) {
  tr <- e1071.tune.result
  inds <- tr$train.ind
  
  size <- max(melt.list(inds)$value)
  folds <- length(inds)

  cv.instance <- make.res.instance("cv", size=size, iters=folds)

  for (i in 1:folds)
    cv.instance@inds[[i]] = inds[[i]]
    
  return (cv.instance)
}


e1071.bs.to.mlr.bs <- function(e1071.tune.result) {
  tr <- e1071.tune.result
  inds <- tr$train.ind

  size <- length(inds[[1]])
  iters <- length(inds)
  
  bs.instance <- make.res.instance("bs", size=size, iters=iters)

  for (i in 1:iters)
	bs.instance@inds[[i]] = inds[[i]]
  
  return (bs.instance)
}

# we check that a warning was generating when evaluation e,
# which contains string w
checkWarning = function(e, w, msg="") {
  assign("mywarn", NULL, envir=.GlobalEnv) 
  # for some reason i cannt always assign the warning out of the calling handler
  x = try( 
      withCallingHandlers(
          e,
          warning = function(w) {
            stop(w$message)
          }
  ), silent=T)
  if (!is(x, "try-error")) {
    ok = identical(w, FALSE)
  } else {
    ok = length(grep(w, x)) > 0 
    print(ok)
  }
  
  if (RUnit:::.existsTestLogger()) {
    .testLogger$incrementCheckNum()
  }
  if (!ok) {
    if (RUnit:::.existsTestLogger()) {
      .testLogger$setFailure()
    }
    stop("Test not TRUE\n", msg)
  } else {
    return(TRUE)
  }
}

