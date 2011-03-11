#' @exportClass CombineWrapperRegrSuperLearner
setClass(
  "CombineWrapperRegrSuperLearner",   
  contains = c("BaseCombiner"),
  representation = representation(
    learners = "list"
  )
)   

#' @export 
makeCombineWrapperRegrSuperLearner = function(learners) {
  new("CombineWrapperRegrSuperLearner", learners=learners, id="RegrSuperLearner", 
    par.set=makeParameterSet(), par.vals=list())
}

#todo: bad method to set properties!!
setMethod(
  f = "[",
  signature = signature("CombineWrapperRegrSuperLearner"),
  def = function(x,i,j,...,drop) {
    if (i == "is.classif")
      return(FALSE)
    if (i %in% c("numerics", "factors")) {
      return(TRUE)
    }
    callNextMethod()
  }
)




#' @rdname trainLearner
setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="CombineWrapperRegrSuperLearner", 
    .task="LearnTask", .subset="integer"
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    .task = subset(.task, subset=.subset)  
    res = makeResampleInstance(.learner@resample.desc, .task)   
    rs = lapply(.learner@learners, function(w) resample(w, .task, res))
  }
)



#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "CombineWrapperRegrSuperLearner", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    models = .model["learner.model"]
    
    p = sapply(models, function(m) predict(m, newdata=.newdata,  ...)@df$response)
    p = rowMeans(p)
  }
)   


