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
  "regr.kmforrester", 
  contains = c("rlearner.regr")
)


setMethod(
  f = "initialize",
  signature = signature("regr.kmforrester"),
  def = function(.Object) {
    
    desc = c(
      missings = FALSE,
      numerics = TRUE,
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
    .learner="regr.kmforrester", 
    .task="RegrTask", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    d = get.data(.task, .subset, target.extra=TRUE)
    d = get.data(.task, .subset, target.extra=TRUE)
    m = km(design=d$data, response=d$target, nugget.estim=TRUE, ...)
    p = predict(m, d$data, type="SK")$mean
    m = km(design=d$data, response=p, nugget.estim=FALSE, 
       coef.trend=m@trend.coef, coef.var=m@covariance@sd2, coef.cov=m@covariance@range.val)
    return(m)   
  }
)

#' @rdname predictLearner

setMethod(
  f = "predictLearner",
  signature = signature(
    .learner = "regr.kmforrester", 
    .model = "WrappedModel", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    p = predict(.model["learner.model"], newdata=.newdata, type="SK", se.compute=FALSE, ...)
    return(p$mean) 
  }
) 
