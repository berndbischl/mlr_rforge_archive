#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.r
roxygen()
#' @include predictLearner.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
  "regr.fnn", 
  contains = c("rlearner.regr")
)

setMethod(
  f = "initialize",
  signature = signature("regr.fnn"),
  def = function(.Object) {
    
    desc = c(
      missings = FALSE,
      doubles = TRUE,
      factors = FALSE,
      weights = FALSE
    )
    
    # l is for reject option. cannot be done with mlr atm
    par.set = makeParameterSet(
      makeIntegerLearnerParameter(id="k", default=1L, lower=1L),
      makeLogicalLearnerParameter(id="use.all", default=TRUE, requires=expression(algorithm == "VR")),
      makeDiscreteLearnerParameter(id="algorithm", default="cover_tree", vals=list("cover_tree", "kd_tree", "VR"))
    )
    callNextMethod(.Object, pack="FNN", desc=desc, par.set=par.set)
  }
)

#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="regr.fnn", 
    .task="RegrTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset, ...) {
    d = get.data(.task, .subset, target.extra=TRUE)
    list(train=d, parset=list(...))
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "regr.fnn", 
    .model = "WrappedModel", 
    .newdata = "data.frame",
    .type="missing" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    m = .model["learner.model"]
    pars = list(train=m$train$data, test=.newdata, y=m$train$target)  
    pars = c(pars, m$parset, list(...))
    do.call(FNN::knn.reg, pars)$pred
  }
)	




