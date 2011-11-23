#todo read doc in LogicReg again and talk to holger about details
#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include RegrTask.R
roxygen()


setClass(
  "regr.logicreg", 
  contains = c("rlearner.regr")
)


setMethod(
  f = "initialize",
  signature = signature("regr.logicreg"),
  def = function(.Object) {
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="ntrees", lower=1L, upper=5L), 
      makeIntegerLearnerParam(id="nleaves", lower=1L), 
      makeNumericLearnerParam(id="penalty", lower=0),
      makeIntegerLearnerParam(id="seed"),
      makeDiscreteLearnerParam(id="select", default=1L, vals=c(1,6), pass.default=TRUE),
      makeIntegerLearnerParam(id="treesize", default=8L, lower=1), 
      makeDiscreteLearnerParam(id="opers", default=1L, vals=1:3), 
      makeIntegerLearnerParam(id="minmass", default=0L, lower=0L) 
    )
    
    .Object = callNextMethod(.Object, pack="LogicReg", par.set=par.set)
  
    setProperties(.Object,
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      weights = TRUE
    )
  }
)

#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="regr.logicreg", 
    .task="RegrTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset, ntrees, nleaves, penalty, seed, select, ...) {
    xs = learnerArgsToControl(logreg.tree.control, c("treesize", "opers", "minmass"), list(...))
    d = getData(.task, .subset, target.extra=TRUE)
    logreg(bin=d$data, resp=d$target, type=2, tree.control=xs$control, 
      select=select, ntrees=ntrees, nleaves=nleaves, penalty=penalty, seed=seed)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "regr.logicreg", 
    .model = "WrappedModel", 
    .newdata = "data.frame" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    predict(.model@learner.model, newbin=.newdata, ...)
  }
) 
