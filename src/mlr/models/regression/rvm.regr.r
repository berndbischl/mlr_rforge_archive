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
    "regr.rvm", 
    contains = c("rlearner.regr")
)


setMethod(
    f = "initialize",
    signature = signature("regr.rvm"),
    def = function(.Object) {
      
      # to do: stringdot pars and check order, scale and offset limits
      par.set = makeParamSet(
          makeDiscreteLearnerParam(id="kernel", default="rbfdot", 
              vals=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
          makeNumericLearnerParam(id="tau", lower=0, default=0.01),
          makeNumericLearnerParam(id="sigma",
              lower=0, requires=expression(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
          makeIntegerLearnerParam(id="degree", default=3L, lower=1L, 
              requires=expression(kernel %in% c("polydot", "anovadot", "besseldot"))),
          makeNumericLearnerParam(id="scale", default=1, lower=0, 
              requires=expression(kernel %in% c("polydot", "tanhdot"))),
          makeNumericLearnerParam(id="offset", default=1, 
              requires=expression(kernel %in% c("polydot", "tanhdot"))),
          makeNumericLearnerParam(id="order", default=1L, 
              requires=expression(kernel == "besseldot")),
          makeNumericLearnerParam(id="alpha", default=5L, lower=0L),
          makeNumericLearnerParam(id="var", default=0.1, lower=0),
          makeLogicalLearnerParam(id="var.fix", default=FALSE),
          makeNumericLearnerParam(id="iterations", default=100L, lower=0L),
          makeNumericLearnerParam(id="tol", default=.Machine$double.eps, lower=0),
      		makeNumericLearnerParam(id="minmaxdiff", default=0.001, lower=0)
      )
      
      .Object = callNextMethod(.Object, pack="kernlab")
      
      setProperties(.Object,
        missings = FALSE,
        numerics = TRUE,
        factors = TRUE,
        weights = FALSE  
      )
    }
)


#' @rdname trainLearner

setMethod(
    f = "trainLearner",
    signature = signature(
        .learner="regr.rvm", 
        .task="RegrTask", .subset="integer" 
    ),
    
    # todo unify cla + regr, test all sigma stuff
    def = function(.learner, .task, .subset,  ...) {
      
      xs = learnerArgsToControl(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda", "normalized"), list(...))
      f = getFormula(.task)
      if (length(xs$control) > 0)
        args = c(list(f, data=getData(.task, .subset), fit=FALSE, kpar=xs$control), xs$args)
      else
        args = c(list(f, data=getData(.task, .subset), fit=FALSE), xs$args)
      do.call(rvm, args)
      
    }
)

#' @rdname predictLearner

setMethod(
    f = "predictLearner",
    signature = signature(
        .learner = "regr.rvm", 
        .model = "WrappedModel", 
        .newdata = "data.frame" 
    ),
    
    def = function(.learner, .model, .newdata, ...) {
      predict(.model@learner.model, newdata=.newdata, ...)
    }
) 

