#' @include learnerR.r
roxygen()

setClass(
  "regr.sg.libsvr", 
  contains = c("rlearner.regr")
)


setMethod(
  f = "initialize",
  signature = signature("regr.sg.libsvr"),
  def = function(.Object) {
    
    desc = new("learner.desc.regr",
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      weights = FALSE
    )
    
    callNextMethod(.Object, pack="sg", desc=desc)
  }
)

#' @rdname train.learner

sg.set.hyper.pars = function(control) {
  sg('set_kernel', 'GAUSSIAN', 'REAL', control$size_cache, control$width)
  sg('svr_tube_epsilon', control$epsilon)
}


setMethod(
  f = "train.learner",
  signature = signature(
    .learner="regr.sg.libsvr", 
    .task="regr.task", .subset="integer", .vars="character" 
  ),
  
  def = function(.learner, .task, .subset, .vars, ...) {
    size_cache = 100
    y = task["data"][.subset, .vars][, .targetvar]
    task["data"][.subset, .vars][, .targetvar] = NULL
    # shogun wants features in as column vectors
    task["data"][.subset, .vars] = t(as.matrix(task["data"][.subset, .vars]))
    pars <<- list(...)
    sg('set_features', 'TRAIN', task["data"][.subset, .vars])
    sg('set_labels', 'TRAIN', y)
    sg('new_regression', pars$type)
    sg.set.hyper.pars(pars)
    sg('train_regression')
    svm = sg('get_svm')
    # todo: saving traindat is very inefficient....
    names(svm) = c("bias", "alphas")
    list(svm=svm, control=pars, traindat=task["data"][.subset, .vars], y=y)
  }
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "regr.sg.libsvr", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    # shogun wants features in as column vectors
    .newdata = t(as.matrix(.newdata))
    m = .model["learner.model"]
    sg('set_features', 'TRAIN', m$traindat)
    sg('set_labels', 'TRAIN', m$y)
    sg('set_features', 'TEST', .newdata)
    sg('set_svm', m$svm$bias, m$svm$alphas)
    ctrl = m$control
    sg.set.hyper.pars(ctrl)
    sg('classify')
  }
)	

