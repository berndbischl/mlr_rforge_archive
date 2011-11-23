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
  "classif.pamr", 
  contains = c("rlearner.classif")
)


setMethod(
  f = "initialize",
  signature = signature("classif.pamr"),
  def = function(.Object) {
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id="threshold"),
      makeIntegerLearnerParam(id="n.threshold", lower=1L, default=30L),
      makeLogicalLearnerParam(id="scale.sd", default=TRUE),
      makeNumericVectorLearnerParam(id="threshold.scale", lower=0),
      makeNumericVectorLearnerParam(id="se.scale", default=5L, lower=0),
      makeDiscreteLearnerParam(id="offset.percent", values=0:2),
      makeUntypedLearnerParam(id="hetero", default=NULL),
      makeNumericVectorLearnerParam(id="prior", lower=0, upper=1),
      makeLogicalLearnerParam(id="remove.zeros", default=TRUE),
      makeUntypedLearnerParam(id="sign.contrast", default="both"),
      makeNumericVectorLearnerParam(id="threshold", when="predict")
    )
    
    .Object = callNextMethod(.Object, pack="pamr", par.set=par.set)
    
    setProperties(.Object, 
      twoclass = TRUE,
      multiclass = TRUE,
      numerics = TRUE,
      prob = TRUE
    )
  }
)

#' @rdname trainLearner


setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="classif.pamr", 
    .task="ClassifTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    f = getFormula(.task)
    data = getData(.task, .subset, target.extra=TRUE)
    names(data)[1] = "x"  
    names(data)[2] = "y"
    data$x = t(data$x)
    pamr.train(data, ...)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "classif.pamr", 
    .model = "WrappedModel", 
    .newdata = "data.frame" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    type = switch(.learner@predict.type, prob="posterior", "class")
    pamr.predict(.model@learner.model, newx=t(.newdata), type=type, threshold=1, ...)
  }
) 





