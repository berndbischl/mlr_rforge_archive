makeOptWrapper = function(learner, resampling, measures, par.set, bit.names, bits.to.features,
  control, show.info) {
  
  checkArg(resampling)
  checkArg(measures, "list")
  checkListElementClass(measures, "Measure")
  checkArg(measures, "lsit")
  checkArg(show.info, "logical", len=1L, na.ok=FALSE)

  x = makeBaseWrapper(learner)
  x$resampling = resampling
  x$measures = measures
  x$opt.pars = par.set
  x$bit.names = bit.names
  x$bits.to.features = bits.to.features
  x$opt.pars = par.set
  x$control = control
  x$show.info = show.info
  # set predict type of base learner
  setPredictType(x, learner$predict.type)
}
