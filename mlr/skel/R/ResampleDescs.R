makeResampleDescHoldout = function(iters, split) {
  makeResampleDescInternal( "holdout", "HoldoutInstance", iters=1L, split=split)
}

makeResampleDescCV = function(iters=10L) {
  makeResampleDescInternal("cross-validation", "CVInstance", iters=iters)
}

makeResampleDescLOO = function(iters) {
  makeResampleDescInternal("LOO", "LOOInstance", iters=as.integer(NA))
}  

makeResampleDescSubsample = function(iters=50L, split=2/3) {
  makeResampleDescInternal("subsampling", "SubsampleInstance", iters=iters, split=split)
}

makeResampleDescBootstrap = function(iters=50L) {
  makeResampleDescInternal("OOB bootstrapping", "BoostrapInstance", iters)
}

makeResampleDescRepCV = function(reps=10L, folds=10L) {
  if (!is.numeric(reps) || length(reps) != 1)
    stop("Argument 'reps' must be integer and of length 1!")
  if (!is.numeric(folds) || length(folds) != 1)
    stop("Argument 'folds' must be integer and of length 1!")
  if (iters != reps * folds)
    stop("Argument 'iters' must be 'reps' x 'folds'")
  .Object$reps=as.integer(reps)
  makeResampleDescInternal("repeated cross-validation", "RepCVInstance", iters=folds*reps, folds=folds, reps=reps)
}

print.SubsampleDesc = function(x, ...) { 
  catf("%s with %i iterations and %.2f split rate.", object$id, object$iters, object$split)
  catf("Predict: %s", object$predict)
}


print.RepCVDesc = function(x, ...) { 
  catf("%s with %i iterations: %i folds and %i reps.", object$id, object$iters, object$iters/object$reps, object$reps)
  catf("Predict: %s", object$predict)
}




