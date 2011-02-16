library(reshape)

e1071.cv.to.mlr.cv <- function(e1071.tune.result) {
  tr <- e1071.tune.result
  inds <- tr$train.ind
  
  size <- max(melt.list(inds)$value)
  folds <- length(inds)
  
  d = makeResampleDesc("cv", iters=folds)
  cv.instance = makeResampleInstance(d, size=size)

  for (i in 1:folds) {
    cv.instance@train.inds[[i]] = inds[[i]]
    cv.instance@test.inds[[i]] = setdiff(1:size, inds[[i]])
  }
  
  return (cv.instance)
}


e1071.bs.to.mlr.bs <- function(e1071.tune.result) {
  tr <- e1071.tune.result
  inds <- tr$train.ind

  size <- length(inds[[1]])
  iters <- length(inds)
  
  d = makeResampleDesc("bs", iters=iters)
  bs.instance = makeResampleInstance(d, size=size)

  for (i in 1:iters) {
	  bs.instance@train.inds[[i]] = inds[[i]]
    bs.instance@test.inds[[i]] = setdiff(1:size, inds[[i]])
  }
  return (bs.instance)
}

# we check that a warning was generating when evaluation e,
# which contains string w
checkWarning = function(e, w, msg="") {
  assign("mywarn", NULL, envir=.GlobalEnv) 
  suppressWarnings( 
      withCallingHandlers(
          e,
          warning = function(w) {
            assign("mywarn", w, envir=.GlobalEnv)  
          }
      ))
  if (is.null(mywarn) ) {
    ok = identical(w, FALSE)
  } else {
    ok = length(grep(w, mywarn$message)) > 0 
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

