#' @exportClass CombineWrapperRegrAvg
setClass(
  "CombineWrapperRegrAvg",   
  contains = c("BaseCombiner"),
  representation = representation(
    learners = "list"
  )
)   

#' @export 
makeCombineWrapperRegrAvg = function(learners) {
  new("CombineWrapperRegrAvg", learners=learners, par.set=list(), par.vals=list())
}

setMethod(
  f = "[",
  signature = signature("CombineWrapperRegrAvg"),
  def = function(x,i,j,...,drop) {
    if (i %in% c("oneclass", "twoclass", "multiclass", "doubles", "factors", "is.classif")) {
      return(T)
    }
    callNextMethod()
  }
)


#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "CombineWrapperRegrAvg", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    models = .model["learner.model"]
    p = sapply(models, function(m) predict(m, newdata=.newdata,  ...)["response"])
    p = colMeans(p)
  }
)   


