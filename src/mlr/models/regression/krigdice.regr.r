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
    "regr.km", 
    contains = c("rlearner.regr")
)


setMethod(
    f = "initialize",
    signature = signature("regr.km"),
    def = function(.Object) {
      
      desc = c(
          missings = FALSE,
          numerics = TRUE,
          factors = FALSE,
          weights = FALSE
      )
       
      par.set = makeParameterSet(
        makeDiscreteLearnerParameter(id="covtype", default="matern5_2", 
          vals=list("gauss", "matern5_2", "matern3_2", "exp", "powexp")) 
      )
      callNextMethod(.Object, pack="DiceKriging", desc=desc, par.set=par.set)
    }
)

#' @rdname trainLearner

setMethod(
    f = "trainLearner",
    signature = signature(
        .learner="regr.km", 
        .task="RegrTask", .subset="integer" 
    ),
    
    def = function(.learner, .task, .subset,  ...) {
      d = get.data(.task, .subset, target.extra=TRUE)
      km(design=d$data, response=d$target, ...)
    }
)

#' @rdname predictLearner

setMethod(
    f = "predictLearner",
    signature = signature(
        .learner = "regr.km", 
        .model = "WrappedModel", 
        .newdata = "data.frame", 
        .type = "missing" 
    ),
    
    def = function(.learner, .model, .newdata, .type, ...) {
      p = predict(.model["learner.model"], newdata=.newdata, type="SK", se.compute=FALSE, ...)
      return(p$mean) 
    }
) 
