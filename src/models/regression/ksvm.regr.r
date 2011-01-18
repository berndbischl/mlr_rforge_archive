#' @include learnerR.r
roxygen()
#' @include task.regr.r
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
					doubles = TRUE,
					factors = TRUE,
					weights = FALSE
			)
      par.descs = list(
        logical.learner.parameter(name="scaled", default=TRUE),
        discrete.learner.parameter(name="type", default="eps-svr", vals=c("eps-svr", "nu-svr", "eps-bsvr")),
        discrete.learner.parameter(name="kernel", default="rbfdot", 
          vals=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
        numeric.learner.parameter(name="C",
          lower=0, default=1, requires=expression(type %in% c("eps-svr", "eps-bsvr"))),
        numeric.learner.parameter(name="nu",
          lower=0, default=0.2, requires=expression(type == "nu-svr")),
        numeric.learner.parameter(name="sigma",
          lower=0, requires=expression(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
        integer.learner.parameter(name="degree", default=3L, lower=1L, 
          requires=expression(kernel %in% c("polydot", "anovadot", "besseldot"))),
        numeric.learner.parameter(name="scale", default=1, lower=0, 
          requires=expression(kernel %in% c("polydot", "tanhdot"))),
        numeric.learner.parameter(name="offset", default=1, 
          requires=expression(kernel %in% c("polydot", "tanhdot"))),
        integer.learner.parameter(name="order", default=1L, 
          requires=expression(kernel == "besseldot")),
        numeric.learner.parameter(name="tol", default=0.001, lower=0),
        logical.learner.parameter(name="shrinking", default=TRUE)
      )
      
			callNextMethod(.Object, pack="kernlab", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.ksvm", 
				.task="regr.task", .subset="integer" 
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

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "regr.ksvm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model["learner.model"], newdata=.newdata, ...)[,1]
		}
)	



