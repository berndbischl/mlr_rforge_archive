makeOptWrapper = function(learner, resampling, measures, par.set, bit.names, bits.to.features,
  control, show.info, cl) {
  
  if (!(inherits(resampling, "ResampleDesc") || inherits(resampling, "ResampleInstance")))
    stopf("'resampling' must be a 'ResampleDesc' or ResampleInstance, not: %s", 
      class(resampling)[1])
  if (missing(measures)) {
    measures = mlr:::default.measures(learner)
  } else {
    if (is(measures, "Measure"))
      measures = list(measures)   
    else
      checkListElementClass(measures, "Measure")
  }
  # fixme checks for featsel
  checkArg(par.set, "ParamSet")
  checkArg(control, "OptControl")
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)

  x = makeBaseWrapper(learner, cl=c(cl, "OptWrapper"))
  x$resampling = resampling
  x$measures = measures
  x$opt.pars = par.set
  x$bit.names = bit.names
  x$bits.to.features = bits.to.features
  x$opt.pars = par.set
  x$control = control
  x$show.info = show.info
  return(x)
}

#' @S3method makeWrappedModel OptWrapper
makeWrappedModel.OptWrapper = function(learner, model, task.desc, subset, features, time) {
  x = NextMethod()
  class(x) = c("TuneModel", "OptModel", class(x))
  return(x)
}

#' @S3method print OptModel
print.OptModel = function(x, ...) {
  mlr:::print.WrappedModel(x)
  cat("\nOptimization result:\n")
  print(x$opt.result)
}
