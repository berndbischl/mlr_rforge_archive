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
    
    desc = c(
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      weights = TRUE
    )
    
    par.set = makeParameterSet(
      makeIntegerLearnerParameter(id="ntrees", lower=1L, upper=5L), 
      makeIntegerLearnerParameter(id="nleaves", lower=1L), 
      makeNumericLearnerParameter(id="penalty", lower=0),
      makeIntegerLearnerParameter(id="seed"),
      makeDiscreteLearnerParameter(id="select", default=1L, vals=c(1,6), pass.default=TRUE),
      makeIntegerLearnerParameter(id="treesize", default=8L, lower=1), 
      makeDiscreteLearnerParameter(id="opers", default=1L, vals=1:3), 
      makeIntegerLearnerParameter(id="minmass", default=0L, lower=0L) 
    )
    callNextMethod(.Object, pack="LogicReg", desc=desc, par.set=par.set)
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
    xs = args.to.control(logreg.tree.control, c("treesize", "opers", "minmass"), list(...))
    d = get.data(.task, .subset, target.extra=TRUE)
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
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    predict(.model@learner.model, newbin=.newdata, ...)
  }
) 
