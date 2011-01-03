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
    "regr.km", 
    contains = c("rlearner.regr")
)


setMethod(
    f = "initialize",
    signature = signature("regr.km"),
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
        .learner="regr.km", 
        .task="regr.task", .subset="integer" 
    ),
    
    def = function(.learner, .task, .subset,  ...) {
      y = task["targets"][.subset]
      km(design=get.data(.task, .subset, with.target=FALSE), response=y, ...)
    }
)

#' @rdname pred.learner

setMethod(
    f = "pred.learner",
    signature = signature(
        .learner = "regr.km", 
        .model = "wrapped.model", 
        .newdata = "data.frame", 
        .type = "missing" 
    ),
    
    def = function(.learner, .model, .newdata, .type, ...) {
      p = predict(.model["learner.model"], newdata=.newdata, type="SK", se.compute=FALSE, ...)
      return(p$mean) 
    }
) 
