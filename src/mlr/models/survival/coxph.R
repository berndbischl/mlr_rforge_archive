#' @include SurvTask.R
roxygen()


setClass(
  "surv.coxph", 
  contains = c("rlearner.surv")
)


setMethod(
  f = "initialize",
  signature = signature("surv.coxph"),
  def = function(.Object) {
    .Object = callNextMethod(.Object, pack="survival")
    
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
    .learner="surv.coxph", 
    .task="SurvTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset, ...) {
    d = getData(.task, .subset, target.extra=TRUE)
    coxph(d$surv~., data=d$data, ...)
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "surv.coxph", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    if (.type == "response") 
      .type = "lp"
    predict(.model@learner.model, newdata=.newdata, type=.type)
  }
) 





