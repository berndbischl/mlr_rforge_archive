#' @include base.wrapper.r


setClass(
  "probth.wrapper",
  contains = c("base.wrapper")
)

setMethod(
  f = "[",
  signature = signature("probth.wrapper"),
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
#' See \code{\link{set.threshold}} for details of thresholding.  
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

make.probth.wrapper = function(learner, classes) {
  if (is.character(learner))
    learner = make.learner(learner)
  if (!learner["is.classif"])
    stop("Only classifiers can be used as base learners!")
  if (learner["predict.type"] != "prob")
    stop("The predict.type of the base learner must be 'prob'!")
  a = as.list(rep(0.5, length(classes)))
  names(a) = paste("probth", classes, sep=".")
  pds = lapply(names(a), function(x) numeric.learner.parameter(id=x, lower=0, upper=1))
  w = new("probth.wrapper", learner=learner, par.descs=pds, par.vals=a)
  set.predict.type(w, "response")
}

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "probth.wrapper", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    p = pred.learner(.learner@learner, .model, .newdata, .type="prob")
    ths = unlist(.learner["par.vals", head=TRUE])
    # remove "probth"    
    names(ths) = sapply(strsplit(names(ths), "\\."), function(x) x[2])
    set.threshold(p, threshold=ths)
  }
) 
