
#' Wrapper class for learners to filter variables. Experimental. Can currently 
#' only filter to manually selected variables. 
#' 
#' @exportClass FilterWrapper
#' @title Wrapper class for learners to filter variables.

#' @exportClass FilterWrapper
setClass(
		"FilterWrapper",
		contains = c("BaseWrapper")
)


#' Fuses a base learner with a filter method. Creates a learner object, which can be
#' used like any other learner object. 
#' Internally Uses \code{\link{varfilter}} before every model fit. 
#' 
#' Look at package FSelector for details on the filter algorithms. 
#' 
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param fw.method [\code{character(1)}] \cr
#'   Filter method. Available are:
#'   linear.correlation, rank.correlation, information.gain, gain.ratio, symmetrical.uncertainty, chi.squared, random.forest.importance, relief, oneR
#' @param fw.threshold [single double] \cr
#'   Only features whose importance value exceed this are selected.  
#' 
#' @return \code{\linkS4class{Learner}}.
#' 
#' @title Fuse learner with filter method.
#' @export
makeFilterWrapper = function(learner, fw.method="information.gain", fw.threshold) {
  if (is.character(learner))
    learner = makeLearner(learner)
  # todo check that for some the inputs have to be all num. or accept error in train and NA in predict?
  ps = makeParameterSet(
    makeDiscreteLearnerParameter(id="fw.method",
      vals=c("linear.correlation", "rank.correlation", "information.gain", "gain.ratio", 
        "symmetrical.uncertainty", "chi.squared", "random.forest.importance", "relief", "oneR")),
    makeNumericLearnerParameter(id="fw.threshold")
  )
	w = new("FilterWrapper", learner=learner, pack="FSelector", par.set=ps, 
    par.vals=list(fw.method=fw.method, fw.threshold=fw.threshold))
  setPredictType(w, learner["predict.type"])
}



#' @rdname trainLearner
setMethod(
		f = "trainLearner",
    signature = signature(
      .learner="FilterWrapper", 
      .task="LearnTask", .subset="integer"
    ),
		
		def = function(.learner, .task, .subset,  ...) {
      pvs = .learner@par.vals 
      .task = subset(.task, subset=.subset)  
      tn = .task@desc@target
      vars = varfilter(.task, pvs$fw.method, pvs$fw.threshold)$vars
      if (length(vars) > 0) {
        .task = subset(.task, vars=vars)  
        # !we have already subsetted!
			  m = trainLearner(.learner@learner, .task, 1:.task["size"], ...)
      } else {
        # !we have already subsetted!
        m = new("novars", targets=getTargets(.task), desc=.task@desc)
      }
      # set the vars as attribute, so we can extract it later 
      attr(m, "filter.result") = vars
      return(m)
		}
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "FilterWrapper", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    .newdata = .newdata[, .model["vars"], drop=FALSE]  
    predictLearner(.learner@learner, .model, .newdata, .type, ...)
  }
) 

