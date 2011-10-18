#' @include ClassifTask.R
roxygen()
#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include ClassifTask.R
roxygen()


setClass(
		"classif.lssvm", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.lssvm"),
		def = function(.Object) {
      # to do: stringdot pars and check order, scale and offset limits
      par.set = makeParameterSet(
        makeLogicalLearnerParameter(id="scaled", default=TRUE),
        makeDiscreteLearnerParameter(id="kernel", default="rbfdot", 
          vals=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
        makeNumericLearnerParameter(id="tau", lower=0, default=0.01),
        makeLogicalLearnerParameter(id="reduced", default=TRUE),
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
        makeNumericLearnerParameter(id="tol", default=0.0001, lower=0)
      )

      .Object = callNextMethod(.Object, pack="kernlab", par.set=par.set)
    
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        numerics = TRUE,
        factors = TRUE
      )
    }
)


#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.lssvm", 
				.task="ClassifTask", .subset="integer" 
		),
		
		# todo custom kernel. freezes? check mailing list
		# todo unify cla + regr, test all sigma stuff
		def = function(.learner, .task, .subset,  ...) {
			
#			# there's a strange behaviour in r semantics here wgich forces this, see do.call and the comment about substitute
#			if (!is.null(args$kernel) && is.function(args$kernel) && !is(args$kernel,"kernel")) {
#				args$kernel = do.call(args$kernel, kpar)	
#			} 
			
			xs = learnerArgsToControl(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda", "normalized"), list(...))
			f = getFormula(.task)
			if (length(xs$control) > 0)
				args = c(list(f, data=getData(.task, .subset), fit=FALSE, kpar=xs$control), xs$args)
			else
				args = c(list(f, data=getData(.task, .subset), fit=FALSE), xs$args)
			do.call(lssvm, args)
			
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.lssvm", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = switch(.learner@predict.type, "response")
			predict(.model@learner.model, newdata=.newdata, type=type, ...)
		}
)	

