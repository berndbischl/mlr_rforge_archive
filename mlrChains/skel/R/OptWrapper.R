makeOptWrapper = function(learner, resampling, measures, par.set, bit.names, bits.to.features,
  control, show.info) {
  
  x = makeBaseWrapper(x, learner, par.set=makeParamSet(), par.vals=list())
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
