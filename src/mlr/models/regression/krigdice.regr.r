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
      par.set = makeParameterSet(
        makeDiscreteLearnerParameter(id="covtype", default="matern5_2", 
          vals=list("gauss", "matern5_2", "matern3_2", "exp", "powexp")), 
        makeNumericLearnerParameter(id="nugget"), 
        makeLogicalLearnerParameter(id="nugget.estim", default=FALSE), 
        makeNumericVectorLearnerParameter(id="noise.var"), 
        makeDiscreteLearnerParameter(id="optim.method", default="BFGS", 
          vals=list("BFGS", "gen")), 
        makeNumericVectorLearnerParameter(id="lower"), 
        makeNumericVectorLearnerParameter(id="upper"), 
        makeUntypedLearnerParameter(id="control")
      )
      
      .Object = callNextMethod(.Object, pack="DiceKriging", par.set=par.set)
      
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
        .learner="regr.km", 
        .task="RegrTask", .subset="integer" 
    ),
    
    def = function(.learner, .task, .subset,  ...) {
      d = getData(.task, .subset, target.extra=TRUE)
      km(design=d$data, response=d$target, ...)
    }
)

#' @rdname predictLearner

setMethod(
    f = "predictLearner",
    signature = signature(
        .learner = "regr.km", 
        .model = "WrappedModel", 
        .newdata = "data.frame" 
    ),
    
    def = function(.learner, .model, .newdata, ...) {
      p = predict(.model@learner.model, newdata=.newdata, type="SK", se.compute=FALSE, ...)
      return(p$mean) 
    }
) 
