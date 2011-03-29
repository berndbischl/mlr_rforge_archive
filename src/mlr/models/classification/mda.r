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
    
    desc = c(
      oneclass = FALSE,
      twoclass = TRUE,
      multiclass = TRUE,
      missings = FALSE,
      numerics = TRUE,
      factors = TRUE,
      decision = FALSE,
      prob = TRUE,
      weights = FALSE,
      costs = FALSE
    )
 
    x = callNextMethod(.Object, pack="mda", desc=desc)
    
    par.set = makeParameterSet(
      makeUntypedLearnerParameter(id="subclasses", default=2L),
      makeIntegerLearnerParameter(id="iter", default=5L, lower=1L),
      makeIntegerLearnerParameter(id="dimension", lower=1L),
      makeDiscreteLearnerParameter(id="method", default="polyreg", 
        vals=list(polyreg=polyreg, mars=mars, bruto=bruto, gen.ridge=gen.ridge)),
      makeLogicalLearnerParameter(id="trace", default=FALSE),
      # change default and pass it to reduce mem
      makeLogicalLearnerParameter(id="keep.fitted", default=FALSE, pass.default=TRUE),
      makeIntegerLearnerParameter(id="tries", default=5L, lower=1L)
    )

    x@par.set = par.set
    return(x)
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
    f = .task["formula"]
    mda(f, data=get.data(.task, .subset), ...)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "classif.mda", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    .type <- ifelse(.type=="response", "class", "posterior")
    predict(.model@learner.model, newdata=.newdata, type=.type, ...)
  }
)	




