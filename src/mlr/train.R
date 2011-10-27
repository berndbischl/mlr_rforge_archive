#' @include LearnTask.R
roxygen()

#' Given a \code{\linkS4class{LearnTask}}, creates a model for the learning machine 
#' which can be used for predictions on new data. 
#'
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{LearnTask}}]\cr 
#'        Specifies learning task.   
#' @param subset [\code{\link{integer}}] \cr 
#'        An index vector specifying the training cases to be used for fitting. By default the complete data set is used. 
#' 
#' @return \code{\linkS4class{WrappedModel}}. 
#'
#' @export
#'
#' @seealso \code{\link{predict}}
#' 
#' @title Train a learning algorithm.
#' @rdname train

setGeneric(
  name = "train",
  def = function(learner, task, subset) {
    if (is.character(learner))
      learner <- makeLearner(learner)
    if (missing(subset))
      subset <- 1:task@desc@size
    if (is.numeric(subset))
      subset = as.integer(subset)
    standardGeneric("train")
  }
)


#' @export
#' @rdname train 
setMethod(
  f = "train",
  
  signature = signature(
    learner="Learner", 
    task="LearnTask", 
    subset="integer" 
  ),
  
  def = function(learner, task, subset) {
    # make sure that pack for learner ist loaded, probably needed when learner is exported        
    require.packs(learner@pack, paste("learner", learner@id))
    
    checkTaskLearner(task, learner)
    
    wl <- learner
    tn <- task@desc@target
    
    # make pars list for train call
    pars = list(.learner=wl, .task=task, .subset=subset)
    # only pass train hyper pars as basic rlearner in ...
    pars = c(pars, getHyperPars(getLeafLearner(wl), "train"))
    
    logger.debug(level="train", "mlr train:", wl@id, "with pars:")
    logger.debug(level="train", getHyperParsString(wl))
    logger.debug(level="train", "on", length(subset), "examples:")
    logger.debug(level="train", subset)
    
    vars = getFeatureNames(task)
    # no vars? then use no vars model
    if (length(vars) == 0) {
      learner.model = new("novars", targets=task@dataenv$data[subset, tn], desc=task@desc)
      time.train = 0
    } else {
      # set the seed
      if(!is.null(.mlr.local$debug.seed)) {
        set.seed(.mlr.local$debug.seed)
        warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
      }
      # for optwrappers we want to see the tuning / varsel logging
      if (.mlr.local$logger.setup$show.learner.output || is(learner, "OptWrapper"))
        fun1 = identity
      else
        fun1 = capture.output
      if (.mlr.local$errorhandler.setup$on.learner.error == "stop")
        fun2 = identity
      else
        fun2 = function(x) try(x, silent=TRUE)
      st = system.time(or <- fun1(learner.model <- fun2(do.call(trainLearner, pars))), gcFirst = FALSE)
      # was there an error during training? maybe warn then
      if(is(learner.model, "try-error") && .mlr.local$errorhandler.setup$on.learner.error == "warn") {
        warning("Could not train the learner: ", as.character(learner.model))
      }
      logger.debug(level="train", or)
      time.train = as.numeric(st[3])
    }
    
    makeWrappedModel(wl, learner.model, task@desc, subset, vars, time.train)
  }
)
