#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()


setClass(
  # name lm is sealed
  "regr.rsm", 
  contains = c("rlearner.regr")
)


setMethod(
  f = "initialize",
  signature = signature("regr.rsm"),
  def = function(.Object) {
    par.set = makeParameterSet(      
      makeDiscreteLearnerParameter(id="modelfun", default="FO", vals=c("FO", "TWI", "SO"), pass.default=TRUE)
    )
    
    .Object = callNextMethod(.Object, pack="rsm", par.set=par.set)
  
    setProperties(.Object,
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      weights = FALSE
    )
  }
)

#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="regr.rsm", 
    .task="RegrTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset, ...) {
    mf = list(...)$modelfun
    vs = paste(getFeatureNames(.task), collapse=",")
    g = function(x) paste(x, "(", vs, ")", sep="") 
    mf = switch(mf,
      FO = g("FO"),
      TWI = paste(g("TWI"), "+", g("FO")),
      SO = g("SO"),
      stop("Unknown modelfun: ", mf)
    )
    f = as.formula(paste(.task@desc@target, "~", mf))
    myargs = list(f, getData(.task, .subset))
    # strange behaviour in rsm forces us to use do.call...
    do.call(rsm, myargs)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "regr.rsm", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    as.numeric(predict(.model@learner.model, newdata=.newdata, ...))
  }
)	





