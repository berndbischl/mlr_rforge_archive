makeRLearner.classif.blackboost = function() {
  makeRLearnerClassif(
    cl = "classif.blackboost",
    package = c("mboost", "party"),
    par.set = makeParamSet( 
      makeDiscreteLearnerParam(id="family", default=Binomial(), values=list(AdaExp=AdaExp(), Binomial=Binomial())),
      makeIntegerLearnerParam(id="mstop", default=100L, lower=1L),
      makeNumericLearnerParam(id="nu", default=0.1, lower=0, upper=1),
      makeDiscreteLearnerParam(id="teststat", default="quad", values=c("quad", "max")),
      makeDiscreteLearnerParam(id="testtype", default="Bonferroni", values=c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id="mincriterion", default=0.95, lower=0, upper=1),
      makeIntegerLearnerParam(id="minsplit", default=20L, lower=1L),
      makeIntegerLearnerParam(id="minbucket", default=7L, lower=1L),
      makeLogicalLearnerParam(id="stump", default=FALSE),
      makeIntegerLearnerParam(id="nresample", default=9999L, lower=1L, requires=expression(testtype=="MonteCarlo")),
      makeIntegerLearnerParam(id="maxsurrogate", default=0L, lower=0L),
      makeIntegerLearnerParam(id="mtry", default=0L, lower=0L),
      makeLogicalLearnerParam(id="savesplitstats", default=TRUE),
      makeIntegerLearnerParam(id="maxdepth", default=0L, lower=0L)
    ), 
    twoclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

trainLearner.classif.blackboost = function(.learner, .task, .subset,  ...) {
  xs = learnerArgsToControl(boost_control, c("mstop", "nu", "risk"), list(...))
  ys = learnerArgsToControl(ctree_control, c("teststat", "testtype", "mincriterion", "maxdepth"), xs$args)
  f = getFormula(.task)
  args = c(list(f, data=getTaskData(.task, .subset), control=xs$control, tree_control=ys$control), ys$args)
  if (.task$task.desc$has.weights)
    args$weights = .task$weights[.subset] 
  do.call(blackboost, args)
}

predictLearner.classif.blackboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata=.newdata, type=type, ...)
  if (.learner$predict.type == "prob") {
    y = matrix(0, ncol=2, nrow=nrow(.newdata))
    colnames(y) = .model$task.desc$class.levels
    y[,1] = p
    y[,2] = 1-p
    return(y)
  } else {
    return(p)
  }
}









