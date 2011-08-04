#' @include SurvTask.R
roxygen()


setClass(
  "surv.rsf", 
  contains = c("rlearner.surv")
)


setMethod(
  f = "initialize",
  signature = signature("surv.rsf"),
  def = function(.Object) {
    .Object = callNextMethod(.Object, pack="randomSurvivalForest")
    
    setProperties(.Object,
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      weights = FALSE
    )
  }
)

#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="surv.rsf", 
    .task="SurvTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset, ...) {
    d = getData(.task, .subset, target.extra=TRUE)
    rsf(d$surv~., data=d$data, ...)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "surv.rsf", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    if (.type == "response") 
      .type = "lp"
    predict(.model@learner.model, newdata=.newdata, type=.type)
  }
) 





