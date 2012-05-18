makeRLearner.regr.rsm = function() {
  makeRLearnerRegr(
    cl = "regr.rsm",
    package = "rsm",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="modelfun", default="FO", values=c("FO", "TWI", "SO"), pass.default=TRUE)
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.rsm = function(.learner, .task, .subset,  ...) {
  mf = list(...)$modelfun
  vs = paste(getFeatureNames(.task), collapse=",")
  g = function(x) paste(x, "(", vs, ")", sep="") 
  mf = switch(mf,
    FO = g("FO"),
    TWI = paste(g("TWI"), "+", g("FO")),
    SO = g("SO"),
    stop("Unknown modelfun: ", mf)
  )
  f = as.formula(paste(.task$desc$target, "~", mf))
  myargs = list(f, getData(.task, .subset))
  # strange behaviour in rsm forces us to use do.call...
  do.call(rsm, myargs)
}

predictLearner.regr.rsm = function(.learner, .model, .newdata, ...) {
  as.numeric(predict(.model$learner.model, newdata=.newdata, ...))
}