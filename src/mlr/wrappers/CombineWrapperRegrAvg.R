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
  new("CombineWrapperRegrAvg", learners=learners, id="CombineWrapperRegrAvg", 
    par.set=makeParameterSet(), par.vals=list())
}

#todo: bad method to set properties!!
setMethod(
  f = "[",
  signature = signature("CombineWrapperRegrAvg"),
  def = function(x,i,j,...,drop) {
    if (i == "is.classif")
      return(FALSE)
    if (i %in% c("numerics", "factors")) {
      return(TRUE)
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
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    models = .model["learner.model"]
    p = sapply(models, function(m) predict(m, newdata=.newdata,  ...)@df$response)
    p = rowMeans(p)
  }
)   


