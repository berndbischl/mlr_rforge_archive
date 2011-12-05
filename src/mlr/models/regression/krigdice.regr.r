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
      par.set = makeParamSet(
        makeDiscreteLearnerParam(id="covtype", default="matern5_2", 
          values=list("gauss", "matern5_2", "matern3_2", "exp", "powexp")), 
        makeNumericLearnerParam(id="nugget"), 
        makeLogicalLearnerParam(id="nugget.estim", default=FALSE), 
        makeNumericVectorLearnerParam(id="noise.var"), 
        makeDiscreteLearnerParam(id="optim.method", default="BFGS", 
          values=list("BFGS", "gen")), 
        makeNumericVectorLearnerParam(id="lower"), 
        makeNumericVectorLearnerParam(id="upper"), 
        makeUntypedLearnerParam(id="control")
      )
      
      .Object = callNextMethod(.Object, pack="DiceKriging", par.set=par.set)
      
      setProperties(.Object,
        missings = FALSE,
        numerics = TRUE,
        factors = FALSE,
        se.fit = TRUE,
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
      if(.learner@predict.type == "response")
        return(p$mean)
      else
        cbind(p$mean, p$se.fit)
    }
) 
