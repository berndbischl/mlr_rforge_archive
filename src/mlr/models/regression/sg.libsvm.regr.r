#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
  "regr.sg.libsvr", 
  contains = c("rlearner.regr")
)


setMethod(
  f = "initialize",
  signature = signature("regr.sg.libsvr"),
  def = function(.Object) {
    .Object = callNextMethod(.Object, pack="sg")
    
    setProperties(.Object,
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      weights = FALSE
    )
  }
)

#' @rdname trainLearner

sg.setHyperPars = function(control) {
  sg('set_kernel', 'GAUSSIAN', 'REAL', control$size_cache, control$width)
  sg('svr_tube_epsilon', control$epsilon)
}


setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="regr.sg.libsvr", 
    .task="RegrTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset, ...) {
    size_cache = 100
    d = getData(.task, .subset, target.extra=TRUE, class.as="-1+1")
    # shogun wants features in as column vectors
    train = t(d(as.matrix(d$data)))
    pars = list(...)
    sg('set_features', 'TRAIN', train)
    sg('set_labels', 'TRAIN', y)
    sg('new_regression', pars$type)
    sg.setHyperPars(pars)
    sg('train_regression')
    svm = sg('get_svm')
    # todo: saving traindat is very inefficient....
    names(svm) = c("bias", "alphas")
    list(svm=svm, control=pars, traindat=train, y=y)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "regr.sg.libsvr", 
    .model = "WrappedModel", 
    .newdata = "data.frame" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    # shogun wants features in as column vectors
    .newdata = t(as.matrix(.newdata))
    m = .model@learner.model
    sg('set_features', 'TRAIN', m$traindat)
    sg('set_labels', 'TRAIN', m$y)
    sg('set_features', 'TEST', .newdata)
    sg('set_svm', m$svm$bias, m$svm$alphas)
    ctrl = m$control
    sg.setHyperPars(ctrl)
    sg('classify')
  }
)	

