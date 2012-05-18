makeResampleDescHoldout = function(iters, split) {
  makeResampleDesc2("HoldoutInstance", "holdout", iters=1L, split=split)
}

makeResampleDescCV = function(iters=10L) {
  makeResampleDesc2("CVInstance", "cross-validation", iters=iters)
}

makeResampleDescLOO = function(iters) {
  makeResampleDesc2("LOOInstance", "LOO", iters=as.integer(NA))
}  

makeResampleDescSubsample = function(iters=50L, split=2/3) {
  makeResampleDesc2("SubsampleInstance", "subsampling", iters=iters, split=split)
}

makeResampleDescBootstrap = function(iters=50L) {
  makeResampleDesc2("BoostrapInstance", "OOB bootstrapping", iters)
}

makeResampleDescRepCV = function(reps=10L, folds=10L) {
  if (!is.numeric(reps) || length(reps) != 1)
    stop("Argument 'reps' must be integer and of length 1!")
  if (!is.numeric(folds) || length(folds) != 1)
    stop("Argument 'folds' must be integer and of length 1!")
  if (iters != reps * folds)
    stop("Argument 'iters' must be 'reps' x 'folds'")
  .Object@reps=as.integer(reps)
  makeResampleDesc2("RepCVInstance", "repeated cross-validation", iters=folds*reps, folds=folds, reps=reps)
}


setMethod("show", "SubsampleDesc", function(object) {
    catf("%s with %i iterations and %.2f split rate.", object@id, object@iters, object@split)
    catf("Predict: %s", object@predict)
  })



print.RepCVDesc = function(x, ...) { 
  setMethod("show", "RepCVDesc", function(object) {
      catf("%s with %i iterations: %i folds and %i reps.", object@id, object@iters, object@iters/object@reps, object@reps)
      catf("Predict: %s", object@predict)
    })
}



