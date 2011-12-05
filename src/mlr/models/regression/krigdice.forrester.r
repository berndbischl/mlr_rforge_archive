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
    .Object = callNextMethod(.Object, pack="DiceKriging")

    setProperties(.Object,
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      se.fit = FALSE,
      weights = FALSE
    )
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
    d = getData(.task, .subset, target.extra=TRUE)
    d = getData(.task, .subset, target.extra=TRUE)
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
    .newdata = "data.frame" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    p = predict(.model@learner.model, newdata=.newdata, type="SK", se.compute=FALSE, ...)
    return(p$mean) 
  }
) 
