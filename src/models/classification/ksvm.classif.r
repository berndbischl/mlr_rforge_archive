#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include task.classif.r
roxygen()


setClass(
		"classif.ksvm", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.ksvm"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = TRUE,
					weights = FALSE,	
					costs = FALSE 
			)

      # to do: stringdot pars and check order, scale and offset limits
      par.descs = list(
          logical.learner.parameter(id="scaled", default=TRUE),
          discrete.learner.parameter(id="type", default="C-svc", vals=c("C-svc", "nu-svc", "C-bsvc", "spoc-svc", "kbb-svc")),
          discrete.learner.parameter(id="kernel", default="rbfdot", 
              vals=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
          numeric.learner.parameter(id="C",
              lower=0, default=1, requires=expression(type %in% c("C-svc", "C-bsvc", "spoc-svc", "kbb-svc"))),
          numeric.learner.parameter(id="nu",
              lower=0, default=0.2, requires=expression(type == "nu-svc")),
          numeric.learner.parameter(id="sigma",
              lower=0, requires=expression(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
          integer.learner.parameter(id="degree", default=3L, lower=1L, 
              requires=expression(kernel %in% c("polydot", "anovadot", "besseldot"))),
          numeric.learner.parameter(id="scale", default=1, lower=0, 
              requires=expression(kernel %in% c("polydot", "tanhdot"))),
          numeric.learner.parameter(id="offset", default=1, 
              requires=expression(kernel %in% c("polydot", "tanhdot"))),
          integer.learner.parameter(id="order", default=1L, 
              requires=expression(kernel == "besseldot")),
          numeric.learner.parameter(id="tol", default=0.001, lower=0),
          logical.learner.parameter(id="shrinking", default=TRUE),
          numeric.learner.parameter(id="class.weights", default=1, lower=0)
      )
      
			callNextMethod(.Object, pack="kernlab", desc=desc, par.descs=par.descs)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.ksvm", 
				.task="classif.task", .subset="integer" 
		),
		
		# todo custom kernel. freezes? check mailing list
		# todo unify cla + regr, test all sigma stuff
		def = function(.learner, .task, .subset,  ...) {
			
#			# there's a strange behaviour in r semantics here wgich forces this, see do.call and the comment about substitute
#			if (!is.null(args$kernel) && is.function(args$kernel) && !is(args$kernel,"kernel")) {
#				args$kernel = do.call(args$kernel, kpar)	
#			} 
			
			xs = args.to.control(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda", "normalized"), list(...))
			f = .task["formula"]
      pm = .learner["predict.type"] == "prob"
			if (length(xs$control) > 0)
				args = c(list(f, data=get.data(.task, .subset), fit=FALSE, kpar=xs$control), xs$args, prob.model=pm)
			else
				args = c(list(f, data=get.data(.task, .subset), fit=FALSE), xs$args, prob.model=pm)
			do.call(ksvm, args)
			
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.ksvm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- switch(.type, prob="probabilities", decision="decision", "response")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	

