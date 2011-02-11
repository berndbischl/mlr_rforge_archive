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
  "regr.km.noisy", 
  contains = c("rlearner.regr")
)


setMethod(
  f = "initialize",
  signature = signature("regr.km.noisy"),
  def = function(.Object) {
    
    desc = c(
      missings = FALSE,
      doubles = TRUE,
      factors = FALSE,
      weights = FALSE
    )
    
    callNextMethod(.Object, pack="DiceKriging", desc=desc)
  }
)

#' @rdname trainLearner

setMethod(
  f = "trainLearner",
  signature = signature(
    .learner="regr.km.noisy", 
    .task="RegrTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    d = get.data(.task, .subset, target.extra=TRUE)
    m = km(design=d$data, response=d$data, nugget.estim=TRUE, ...)
    m = km(design=d$data, response=d$data, nugget.estim=FALSE, 
      coef.trend=m@trend.coef, coef.var=m@covariance@sd2, coef.cov=m@covariance@range.val)   
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "regr.km.noisy", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    p = predict(.model["learner.model"], newdata=.newdata, type="SK", se.compute=FALSE, ...)
    return(p$mean) 
  }
) 
