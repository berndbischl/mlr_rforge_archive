setClass(
  "CombineWrapper",   
  contains = c("Learner"),
  representation = representation(
    learners = "list"
  )
)   

#todo: names of hyperpars should start with "cw."

makeCombineWrapper = function(learners) {
  w = new("CombineWrapper")
  w@learners=learners
  a = as.list(rep(0.5, length(learners)))
  names(a) = paste("", sapply(learners, function(x) x@id), sep="")
  pds = lapply(names(a), function(x) makeNumericLearnerParameter(id=x, lower=0, upper=1))
  w@par.set = pds
  w@par.vals = a
  w@predict.type = "prob"
  w
}


setMethod(
  f = "[",
  signature = signature("CombineWrapper"),
  def = function(x,i,j,...,drop) {
    if (i %in% c("oneclass", "twoclass", "multiclass", "doubles", "factors", "is.classif")) {
      return(T)
    }
    callNextMethod()
  }
)


#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="CombineWrapper", 
    .task="ClassifTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    learners = .learner@learners
    models = list()
    for (i in 1:length(learners)) {
      w = learners[[i]]
      models[[i]] = train(w, .task, .subset)
    }
    return(models)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "CombineWrapper", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    a = unlist(.learner["par.vals", head=TRUE])
    models = .model["learner.model"]
    k = length(models)
    p = matrix(0, nrow(.newdata), ncol=.model@desc["class.nr"])
    levs = .model@desc["class.levels"]
    colnames(p) = levs
    for (i in 1:k) {
      m = models[[i]]
      b = a[m@learner@id]
      p = p + b * getScore(predict(m, newdata=.newdata,  ...), levs)
    }
    p
  }
)   

#' @rdname to.string
setMethod(f = "to.string",
  signature = signature("CombineWrapper"),
  def = function(x) {
    return(paste(
        "",
        sep =""         
      ))
  })



