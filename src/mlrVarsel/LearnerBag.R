setClass(
  "LearnerBag",   
  contains = c("Learner"),
  representation = representation(
    learners = "list"
  )
)   

#todo: names of hyperpars should start with "cw."

makeLearnerBag = function(learners) {
  learners = lapply(learners, function(x) {
      if (is.character(x))
        makeLearner(x)
      else
        x
    }
  )
  ns = sapply(learners, function(x) x@id)
  names(learners) = ns
  w = new("LearnerBag")
  w@learners = learners
  w@par.set = makeParamSet(
    makeDiscreteLearnerParam(id="sel.learner", vals=ns, default=ns[1])
  )
  w@par.vals = list(sel.learner=ns[1])
  w@properties["type"] = "classif"  
  w = setProperties(w, 
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = TRUE
  )
  w@predict.type = "response"
  w
}

#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="LearnerBag", 
    .task="ClassifTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    w = .learner@learners[[getHyperPars(.learner)$sel.learner]]
    train(w, .task, .subset)
  }
)

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "LearnerBag", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    w = .learner@learners[[getHyperPars(.learner)$sel.learner]]
    predictLearner(w, .model@learner.model, .newdata, .type, ...)
  }
)   


#w = makeLearnerBag(list("classif.lda", "classif.rpart"))
#task = makeClassifTask(data=iris, target="Species")
#m = train(w, task)
#p = predict(m, task)
#print(p)
