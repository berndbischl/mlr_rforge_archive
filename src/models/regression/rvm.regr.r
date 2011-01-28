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
    "regr.rvm", 
    contains = c("rlearner.regr")
)


setMethod(
    f = "initialize",
    signature = signature("regr.rvm"),
    def = function(.Object) {
      
      desc = c(
          missings = FALSE,
          doubles = TRUE,
          factors = TRUE,
          weights = FALSE  
      )
      
      # to do: stringdot pars and check order, scale and offset limits
      par.set = list(
          discrete.learner.parameter(id="kernel", default="rbfdot", 
              vals=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
          numeric.learner.parameter(id="tau", lower=0, default=0.01),
          numeric.learner.parameter(id="sigma",
              lower=0, requires=expression(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
          integer.learner.parameter(id="degree", default=3L, lower=1L, 
              requires=expression(kernel %in% c("polydot", "anovadot", "besseldot"))),
          numeric.learner.parameter(id="scale", default=1, lower=0, 
              requires=expression(kernel %in% c("polydot", "tanhdot"))),
          numeric.learner.parameter(id="offset", default=1, 
              requires=expression(kernel %in% c("polydot", "tanhdot"))),
          numeric.learner.parameter(id="order", default=1L, 
              requires=expression(kernel == "besseldot")),
          numeric.learner.parameter(id="alpha", default=5L, lower=0L),
          numeric.learner.parameter(id="var", default=0.1, lower=0),
          logical.learner.parameter(id="var.fix", default=FALSE),
          numeric.learner.parameter(id="iterations", default=100L, lower=0L),
          numeric.learner.parameter(id="tol", default=.Machine$double.eps, lower=0),
      		numeric.learner.parameter(id="minmaxdiff", default=0.001, lower=0)
      )
      
      callNextMethod(.Object, pack="kernlab", desc=desc)
    }
)


#' @rdname train.learner

setMethod(
    f = "train.learner",
    signature = signature(
        .learner="regr.rvm", 
        .task="regr.task", .subset="integer" 
    ),
    
    # todo unify cla + regr, test all sigma stuff
    def = function(.learner, .task, .subset,  ...) {
      
      xs = args.to.control(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda", "normalized"), list(...))
      f = .task["formula"]
      if (length(xs$control) > 0)
        args = c(list(f, data=get.data(.task, .subset), fit=FALSE, kpar=xs$control), xs$args)
      else
        args = c(list(f, data=get.data(.task, .subset), fit=FALSE), xs$args)
      do.call(rvm, args)
      
    }
)

#' @rdname pred.learner

setMethod(
    f = "pred.learner",
    signature = signature(
        .learner = "regr.rvm", 
        .model = "wrapped.model", 
        .newdata = "data.frame", 
        .type = "missing" 
    ),
    
    def = function(.learner, .model, .newdata, .type, ...) {
      predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
    }
) 

