#' Train a learning algorithm.
#'
#' Given a \code{\link{SupervisedTask}}, creates a model for the learning machine 
#' which can be used for predictions on new data. 
#'
#' @param learner [\code{\link{Learner}}]\cr 
#'   The learner.  
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task.   
#' @param subset [\code{integer}]\cr 
#'   An index vector specifying the training cases to be used for fitting. 
#'   By default the complete data set is used. 
#' @return [\code{\link{WrappedModel}}]. 
#' @export
#' @seealso \code{\link{predict}}
train = function(learner, task, subset) {
  checkArg(learner, "Learner")
  checkArg(learner, "SupervisedTask")
  if (missing(subset)) {
    subset = 1:task$desc$size
  } else {
    subset = convertIntegers(subset)
    checkArg(subset, "integer", na.ok=FALSE)
  }  
    
  # make sure that pack for learner ist loaded, probably needed when learner is exported        
  requirePackages(learner$package, paste("learner", learner$id))
  
  checkTaskLearner(task, learner)
  
  tn = task$desc$target
  
  # make pars list for train call
  pars = list(.learner=learner, .task=task, .subset=subset)
  # only pass train hyper pars as basic rlearner in ...
  pars = c(pars, getHyperPars(getLeafLearner(learner), "train"))
  
  vars = getFeatureNames(task)
  # no vars? then use no vars model
  if (length(vars) == 0) {
    learner.model = makeNoFeaturesModel(targets=task$env$data[subset, tn], desc=task$desc)
    time.train = 0
  } else {
    # set the seed
    debug.seed = getOptions(mlr.debug.seed, "NULL")
    if(!is.null(debug.seed))
      set.seed(.mlr.conf$debug.seed)
    # for optwrappers we want to see the tuning / varsel logging
    # FIXME is case really ok for optwrapper? can we supppress then too?
    if (getOptions("mlr.show.learner.output") || is(learner, "OptWrapper"))
      fun1 = identity
    else
      fun1 = capture.output
    if (.mlr.conf$errorhandler.setup$on.learner.error == "stop")
      fun2 = identity
    else
      fun2 = function(x) try(x, silent=TRUE)
    st = system.time(or <- fun1(learner.model <- fun2(do.call(trainLearner, pars))), gcFirst = FALSE)
    # was there an error during training? maybe warn then
    if(is.error(learner.model) && getOptions("mlr.on.learner.error") == "warn") {
      warning("Could not train the learner: ", as.character(learner.model))
    }
    time.train = as.numeric(st[3])
  }
  makeWrappedModel(learner, learner.model, task$desc, subset, vars, time.train)
}
