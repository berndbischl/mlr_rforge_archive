#' @S3method makeRLearner classif.mda
makeRLearner.classif.mda = function() {
  makeRLearnerClassif(
    cl = "classif.mda",
    package = "mda",
    # FIXME: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeUntypedLearnerParam(id="subclasses", default=2L),
      makeIntegerLearnerParam(id="iter", default=5L, lower=1L),
      makeIntegerLearnerParam(id="dimension", lower=1L),
      makeDiscreteLearnerParam(id="method", default=polyreg, 
        values=list(polyreg=polyreg, mars=mars, bruto=bruto, gen.ridge=gen.ridge)),
      makeLogicalLearnerParam(id="trace", default=FALSE),
      makeLogicalLearnerParam(id="keep.fitted", default=TRUE),
      makeIntegerLearnerParam(id="tries", default=5L, lower=1L)
    ), 
    par.vals = list(keep.fitted=FALSE),
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

trainLearner.classif.mda = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  mda(f, data=getTaskData(.task, .subset), ...)
}

predictLearner.classif.mda = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type=="response", "class", "posterior")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}


