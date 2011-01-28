setClass(
  "combine.wrapper",   
  contains = c("learner"),
  representation = representation(
    learners = "list"
  )
)   

#todo: names of hyperpars should start with "cw."

make.combine.wrapper = function(learners) {
  w = new("combine.wrapper")
  w@learners=learners
  a = as.list(rep(0.5, length(learners)))
  names(a) = paste("", sapply(learners, function(x) x["id"]), sep="")
  pds = lapply(names(a), function(x) makeNumericLearnerParameter(id=x, lower=0, upper=1))
  w@par.set = pds
  w@par.vals = a
  w@predict.type = "prob"
  w
}


setMethod(
  f = "[",
  signature = signature("combine.wrapper"),
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
    .learner="combine.wrapper", 
    .task="classif.task", .subset="integer" 
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

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "combine.wrapper", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    a = unlist(.learner["par.vals", head=TRUE])
    models = .model["learner.model"]
    k = length(models)
    p = matrix(0, nrow(.newdata), ncol=.model["desc"]["class.nr"])
    levs = .model["desc"]["class.levels"]
    colnames(p) = levs
    for (i in 1:k) {
      m = models[[i]]
      b = a[m@learner["id"]]
      p = p + b * predict(m, newdata=.newdata,  ...)["prob"][, levs]
    }
    p
  }
)   

#' @rdname to.string
setMethod(f = "to.string",
  signature = signature("combine.wrapper"),
  def = function(x) {
    return(paste(
        "",
        sep =""         
      ))
  })



source("D:\\sync\\projekte\\mlr\\src\\test2.r")


