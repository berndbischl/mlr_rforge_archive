#' @exportClass CombineWrapperRegrSuperLearner
setClass(
  "BaggingWrapper",   
  contains = c("BaseWrapper"),
  representation = representation(
    resampling = "ResampleDesc"
  )
)   

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("BaggingWrapper"),
  def = function(.Object, learner, resampling) {
    if (missing(learner))
      return(make.empty(.Object))
    .Object@resampling = resampling
    .Object = callNextMethod(.Object, learner, par.set=makeParameterSet(), par.vals=list())
    # set predict type of base learner
    setPredictType(.Object, learner@predict.type)
  }
)


#' @export 
makeBaggingWrapper = function(learner, resampling) {
  if (is.character(learner))
    learner = makeLearner(learner)
  new("BaggingWrapper", learner=learner, resampling=resampling)
}

#' @rdname trainLearner
setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="BaggingWrapper", 
    .task="LearnTask", .subset="integer"
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    .task = subset(.task, subset=.subset)  
    res2 = makeResampleInstance(.learner@resampling, task=.task)
    bl = .learner@learner
    f = function(i) {
      inds = res2@train.inds[[i]]
      train(bl, .task, subset=inds)
    }
    iters = res2@desc@iters
    models = mylapply(1:iters, f)
  }
)



#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "BaggingWrapper", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    models = .model@learner.model
    p = sapply(models, function(m) predict(m, newdata=.newdata,  ...)@df$response)
    is.learner@learner
    p = rowMeans(p)
  }
)   


