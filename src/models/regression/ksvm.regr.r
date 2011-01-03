#' @include learnerR.r
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
			
			callNextMethod(.Object, pack="kernlab", desc=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.ksvm", 
				.task="regr.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars, ...) {
			xs = args.to.control(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda"), list(...))
			f = .task["formula"]
			# difference in missing(kpar) and kpar=list()!
			if (length(xs$control) > 0)
				args = c(list(f, data=get.data(.task, .subset, .vars), fit=FALSE, kpar=xs$control), xs$args)
			else
				args = c(list(f, data=get.data(.task, .subset, .vars), fit=FALSE), xs$args)
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



