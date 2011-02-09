setClass(
  "CombineWrapperRegrAvg",   
  contains = c("BaseCombiner"),
  representation = representation(
    learners = "list"
  )
)   

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


#' @rdname train.learner

setMethod(
  f = "train.learner",
  signature = signature(
    .learner="CombineWrapperRegrAvg", 
    .task="ClassifTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    lapply(.learner@learners, function(w) train(w, .task, .subset))
  }
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "CombineWrapperRegrAvg", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    models = .model["learner.model"]
    p = sapply(models, function(m) predict(m, newdata=.newdata,  ...)["response"])
    p = colMeans(p)
  }
)   


