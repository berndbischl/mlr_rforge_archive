#' @include BaseWrapper.R


setClass(
  "ProbthWrapper",
  contains = c("BaseWrapper")
)

setMethod(
  f = "[",
  signature = signature("ProbthWrapper"),
  def = function(x,i,j,...,drop) {
    if (i == "prob")
      return(FALSE)
    if (i == "decision")
      return(FALSE)
    callNextMethod()
  }
)

#' Fuses a classifier with thresholding. Creates a learner object, which can be
#' used like any other learner object, but which produces discrete class labels 
#' from probailities of teh base learner according to thresholds.
#' These thresholds are additional hyperparameters of the new learner and 
#' can therefore be tuned. 
#' 
#' See \code{\link{setThreshold}} for details of thresholding.  
#'
#' @param learner [\code{\linkS4class{learner}}]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param classes [character] \cr
#'   Classes of future classification task.
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with probability thresholding.
#' @export

makeProbthWrapper = function(learner, classes) {
  if (is.character(learner))
    learner = makeLearner(learner)
  if (!learner["is.classif"])
    stop("Only classifiers can be used as base learners!")
  if (learner["predict.type"] != "prob")
    stop("The predict.type of the base learner must be 'prob'!")
  a = as.list(rep(0.5, length(classes)))
  names(a) = paste("probth", classes, sep=".")
  ps = do.call(makeParameterSet, 
    lapply(names(a), function(x) makeNumericLearnerParameter(id=x, lower=0, upper=1)))
  w = new("ProbthWrapper", learner=learner, par.set=ps, par.vals=a)
  setPredictType(w, "response")
}

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "ProbthWrapper", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    p = predictLearner(.learner@learner, .model, .newdata, .type="prob")
    ths = unlist(.learner["par.vals", head=TRUE])
    # remove "probth"    
    names(ths) = sapply(strsplit(names(ths), "\\."), function(x) x[2])
    setThreshold(p, threshold=ths)
  }
) 
