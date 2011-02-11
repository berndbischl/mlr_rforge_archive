#' @include learnerR.r
roxygen()
#' @include task.regr.r
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
    
    desc = c(
      missings = FALSE,
      doubles = TRUE,
      factors = FALSE,
      weights = FALSE
    )
    
    par.set = makeParameterSet(      
      makeDiscreteLearnerParameter(id="modelfun", default="FO", vals=c("FO", "TWI", "SO"), flags=list(pass.default=TRUE))
    )
    
    callNextMethod(.Object, pack="rsm", desc=desc, par.set=par.set)
  }
)

#' @rdname train.learner

setMethod(
  f = "train.learner",
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
    f = as.formula(paste(.task["target"], "~", mf))
    myargs = list(f, get.data(.task, .subset))
    # strange behaviour in rsm forces us to use do.call...
    do.call(rsm, myargs)
  }
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "regr.rsm", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    as.numeric(predict(.model["learner.model"], newdata=.newdata, ...))
  }
)	




