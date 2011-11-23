#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include ClassifTask.R
roxygen()


setClass(
  "classif.JRip", 
  contains = c("rlearner.classif")
)


setMethod(
  f = "initialize",
  signature = signature("classif.JRip"),
  def = function(.Object) {
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="F", default=3L, lower=2L),
      makeNumericLearnerParam(id="N", default=2, lower=0),
      makeIntegerLearnerParam(id="O", default=2L, lower=1L),
      makeLogicalLearnerParam(id="E", default=FALSE),
      makeLogicalLearnerParam(id="P", default=FALSE)
    )      
    
    .Object = callNextMethod(.Object, pack="RWeka", par.set=par.set)
    
    setProperties(.Object, 
      oneclass = FALSE,
      twoclass = TRUE,
      multiclass = TRUE,
      missings = TRUE,
      numerics = TRUE,
      factors = TRUE,
      prob = TRUE,
      weights = FALSE
    )
  }
)

#' @rdname trainLearner


setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="classif.JRip", 
    .task="ClassifTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    f = getFormula(.task)
    ctrl = Weka_control(..., S=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
    JRip(f, data=getData(.task, .subset), control=ctrl)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "classif.JRip", 
    .model = "WrappedModel", 
    .newdata = "data.frame" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    type = switch(.learner@predict.type, prob="prob", "class")
    predict(.model@learner.model, newdata=.newdata, type=type, ...)
  }
)