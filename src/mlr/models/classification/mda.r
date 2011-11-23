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
  "classif.mda", 
  contains = c("rlearner.classif")
)


setMethod(
  f = "initialize",
  signature = signature("classif.mda"),
  def = function(.Object) {
    .Object = callNextMethod(.Object, pack="mda")
    
    .Object = setProperties(.Object, 
      twoclass = TRUE,
      multiclass = TRUE,
      numerics = TRUE,
      factors = TRUE,
      prob = TRUE
    )
    
    par.set = makeParamSet(
      makeUntypedLearnerParam(id="subclasses", default=2L),
      makeIntegerLearnerParam(id="iter", default=5L, lower=1L),
      makeIntegerLearnerParam(id="dimension", lower=1L),
      makeDiscreteLearnerParam(id="method", default="polyreg", 
        values=list(polyreg=polyreg, mars=mars, bruto=bruto, gen.ridge=gen.ridge)),
      makeLogicalLearnerParam(id="trace", default=FALSE),
      # change default and pass it to reduce mem
      makeLogicalLearnerParam(id="keep.fitted", default=FALSE, pass.default=TRUE),
      makeIntegerLearnerParam(id="tries", default=5L, lower=1L)
    )
    .Object@par.set = par.set
    return(.Object)
  }
)

#' @rdname trainLearner


setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="classif.mda", 
    .task="ClassifTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    f = getFormula(.task)
    mda(f, data=getData(.task, .subset), ...)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "classif.mda", 
    .model = "WrappedModel", 
    .newdata = "data.frame" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    type = ifelse(.learner@predict.type=="response", "class", "posterior")
    predict(.model@learner.model, newdata=.newdata, type=type, ...)
  }
)	




