#' @include LearnTask.R
roxygen()

#' Given a \code{\linkS4class{LearnTask}}, creates a model for the learning machine 
#' which can be used for predictions on new data. 
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
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
				subset <- 1:task["size"]
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
				learner="learner", 
				task="LearnTask", 
				subset="integer" 
		),
		
		def = function(learner, task, subset) {
      
      # todo: do we still need this, and the loading when exporting a learner? 
      # pack is loaded when learner is constructed
      # export: probably yes...
      require.packs(learner["pack"], paste("learner", learner@id))
      
      check.result = if (is(task, "ClassifTask")) check.task.learner.classif(task, learner) else check.task.learner(task, learner)
      
      if (check.result$msg != "") {
        stop(check.result$msg)
      }
      
      wl <- learner
      tn <- task["target"]
                
      # make pars list for train call
      pars = list(.learner=wl, .task=task, .subset=subset)
      # only pass train hyper pars as basic rlearner in ...
      pars = c(pars, wl["leaf.learner"]["par.train"])
      
      logger.debug(level="train", "mlr train:", wl@id, "with pars:")
      logger.debug(level="train", wl["par.vals.string"])
      logger.debug(level="train", "on", length(subset), "examples:")
      logger.debug(level="train", subset)
      
      vars = getFeatureNames(task)
      # no vars? then use no vars model
      if (length(vars) == 0) {
        learner.model = new("novars", targets=task["data"][subset, tn], desc=task@desc)
        time.train = 0
      } else {
        # set the seed
        if(!is.null(.mlr.local$debug.seed)) {
          set.seed(.mlr.local$debug.seed)
          warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
        }
        
        st = system.time(or <- capture.output({
              if (.mlr.local$errorhandler.setup$on.learner.error == "stop")
                learner.model <- do.call(trainLearner, pars)
              else
                learner.model <- try(do.call(trainLearner, pars), silent=TRUE)
            }), gcFirst = FALSE)
        logger.debug(level="train", or)
        time.train = as.numeric(st[3])
      }
      
      makeWrappedModel(wl, learner.model, task@desc, task@control, subset, vars, time.train)
		}
)
