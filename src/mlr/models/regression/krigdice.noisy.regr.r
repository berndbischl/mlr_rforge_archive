#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include task.regr.r
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

#' @rdname train.learner

setMethod(
  f = "train.learner",
  signature = signature(
    .learner="regr.km.noisy", 
    .task="regr.task", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    d = get.data(.task, .subset, target.extra=TRUE)
    m = km(design=d$data, response=d$data, nugget.estim=TRUE, ...)
    m = km(design=d$data, response=d$data, nugget.estim=FALSE, 
      coef.trend=m@trend.coef, coef.var=m@covariance@sd2, coef.cov=m@covariance@range.val)   
  }
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "regr.km.noisy", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "missing" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    p = predict(.model["learner.model"], newdata=.newdata, type="SK", se.compute=FALSE, ...)
    return(p$mean) 
  }
) 
