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
  "regr.fnn", 
  contains = c("rlearner.regr")
)

setMethod(
  f = "initialize",
  signature = signature("regr.fnn"),
  def = function(.Object) {
    # l is for reject option. cannot be done with mlr atm
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="k", default=1L, lower=1L),
      makeLogicalLearnerParam(id="use.all", default=TRUE, requires=expression(algorithm == "VR")),
      makeDiscreteLearnerParam(id="algorithm", default="cover_tree", values=list("cover_tree", "kd_tree", "VR"))
    )
    
    .Object = callNextMethod(.Object, pack="FNN", par.set=par.set)

    setProperties(.Object,
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      se = FALSE,
      weights = FALSE
    )
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
    d = getData(.task, .subset, target.extra=TRUE)
    list(train=d, parset=list(...))
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "regr.fnn", 
    .model = "WrappedModel", 
    .newdata = "data.frame"
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    m = .model@learner.model
    pars = list(train=m$train$data, test=.newdata, y=m$train$target)  
    pars = c(pars, m$parset, list(...))
    do.call(FNN::knn.reg, pars)$pred
  }
)	




