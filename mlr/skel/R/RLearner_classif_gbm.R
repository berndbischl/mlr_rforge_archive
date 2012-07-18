#todo: probs can only be predicted for two class problems (winning class)

makeRLearner.classif.gbm = function() {
  makeRLearnerClassif(
    cl = "classif.gbm",
    package = "gbm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="distribution", default="bernoulli", values=c("bernoulli", "adaboost")),
      makeIntegerLearnerParam(id="n.trees", default=100L, lower=1L),
      makeIntegerLearnerParam(id="interaction.depth", default=1L, lower=1L),
      makeIntegerLearnerParam(id="n.minobsinnode", default=10L, lower=1L),
      makeNumericLearnerParam(id="shrinkage", default=0.001, lower=0),
      makeNumericLearnerParam(id="bag.fraction", default=0.5, lower=0, upper=1),
      makeNumericLearnerParam(id="train.fraction", default=1, lower=0, upper=1)
    ), 
    twoclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

trainLearner.classif.gbm = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset, class.as="01")
  if (.task$task.desc$has.weights)
    gbm(f, data=d, keep.data=FALSE, verbose=FALSE, weights=.task$weights[.subset], ...)
  else  
    gbm(f, data=d, keep.data=FALSE, verbose=FALSE, ...)
}

predictLearner.classif.gbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = predict(m, newdata=.newdata, type="response", n.trees=length(m$trees), single.tree=FALSE, ...)
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  if (.learner$predict.type == "prob") {
    y = matrix(0, ncol=2, nrow=nrow(.newdata))
    colnames(y) = levs
    y[,1] = 1-p
    y[,2] = p
    return(y)
  } else {
    p = as.factor(ifelse(p > 0.5, levs[2], levs[1]))
    names(p) = NULL
    return(p)
  }
}