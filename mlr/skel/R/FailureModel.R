



print.FailureModel = function(x, ...) {
  print.WrappedModel(x)
  catf("Training failed: ", x$learner.model)
}
