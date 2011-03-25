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
    
    desc = c(
      oneclass = FALSE,
      twoclass = TRUE,
      multiclass = TRUE,
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      prob = TRUE,
      decision = FALSE,
      weights = FALSE,
      costs = FALSE
    )
    par.set = makeParameterSet(
      makeNumericVectorLearnerParameter(id="threshold"),
      makeIntegerLearnerParameter(id="n.threshold", lower=1L, default=30L),
      makeLogicalLearnerParameter(id="scale.sd", default=TRUE),
      makeNumericVectorLearnerParameter(id="threshold.scale", lower=0),
      makeNumericVectorLearnerParameter(id="se.scale", default=5L, lower=0),
      makeDiscreteLearnerParameter(id="offset.percent", vals=0:2),
      makeUntypedLearnerParameter(id="hetero", default=NULL),
      makeNumericVectorLearnerParameter(id="prior", lower=0, upper=1),
      makeLogicalLearnerParameter(id="remove.zeros", default=TRUE),
      makeUntypedLearnerParameter(id="sign.contrast", default="both"),
      makeNumericVectorLearnerParameter(id="threshold", when="predict")
    )
    
    callNextMethod(.Object, pack="pamr", desc=desc, par.set=par.set)
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
    f = .task["formula"]
    data = get.data(.task, .subset, target.extra=TRUE)
    names(data)[1] = "x"  
    names(data)[2] = "y"
    data$x = t(data$x)
    xx <<- x
    pamr.train(data, ...)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "classif.pamr", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    .type = switch(.type, prob="posterior", "class")
    pamr.predict(.model@learner.model, newx=t(.newdata), type=.type, threshold=1, ...)
  }
) 





