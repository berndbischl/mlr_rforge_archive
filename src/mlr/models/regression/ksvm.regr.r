#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

  
setClass(
		"regr.ksvm", 
		contains = c("rlearner.regr")
)



setMethod(
		f = "initialize",
		signature = signature("regr.ksvm"),
		def = function(.Object) {
			
			desc = c(
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					weights = FALSE
			)
      par.set = makeParameterSet(
        makeLogicalLearnerParameter(id="scaled", default=TRUE),
        makeDiscreteLearnerParameter(id="type", default="eps-svr", vals=c("eps-svr", "nu-svr", "eps-bsvr")),
        makeDiscreteLearnerParameter(id="kernel", default="rbfdot", 
          vals=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
        makeNumericLearnerParameter(id="C",
          lower=0, default=1, requires=expression(type %in% c("eps-svr", "eps-bsvr"))),
        makeNumericLearnerParameter(id="nu",
          lower=0, default=0.2, requires=expression(type == "nu-svr")),
        makeNumericLearnerParameter(id="epsilon", lower=0, default=0.1),
        makeNumericLearnerParameter(id="sigma",
          lower=0, requires=expression(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
        makeIntegerLearnerParameter(id="degree", default=3L, lower=1L, 
          requires=expression(kernel %in% c("polydot", "anovadot", "besseldot"))),
        makeNumericLearnerParameter(id="scale", default=1, lower=0, 
          requires=expression(kernel %in% c("polydot", "tanhdot"))),
        makeNumericLearnerParameter(id="offset", default=1, 
          requires=expression(kernel %in% c("polydot", "tanhdot"))),
        makeIntegerLearnerParameter(id="order", default=1L, 
          requires=expression(kernel == "besseldot")),
        makeNumericLearnerParameter(id="tol", default=0.001, lower=0),
        makeLogicalLearnerParameter(id="shrinking", default=TRUE)
      )
      
			callNextMethod(.Object, pack="kernlab", desc=desc, par.set=par.set)
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.ksvm", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset, ...){
			xs = args.to.control(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda"), list(...))
			f = .task["formula"]
			# difference in missing(kpar) and kpar=list()!
			if (length(xs$control) > 0)
				args = c(list(f, data=get.data(.task, .subset), fit=FALSE, kpar=xs$control), xs$args)
			else
				args = c(list(f, data=get.data(.task, .subset), fit=FALSE), xs$args)
			do.call(ksvm, args)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.ksvm", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model@learner.model, newdata=.newdata, ...)[,1]
		}
)	



