# todo: we could pass costs with extra loss function?

#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include task.classif.r
roxygen()


setClass(
		"classif.glmboost", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.glmboost"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)
			# cannot pass the function Binomial without lopading the package in the super constructor...
			x = callNextMethod(.Object, pack="mboost", desc=desc)
			par.set = makeParameterSet(
					makeDiscreteLearnerParameter(id="family", default="Binomial", vals=list(AdaExp=AdaExp(), Binomial=Binomial())),
          makeIntegerLearnerParameter(id="mstop", default=100L, lower=1L),
					makeNumericLearnerParameter(id="nu", default=0.1, lower=0, upper=1),				
					makeLogicalLearnerParameter(id="center", default=FALSE)
			)
			x@par.set = par.set
			setHyperPars(x, family="Binomial")
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.glmboost", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			xs = args.to.control(boost_control, c("mstop", "nu", "risk"), list(...))
			f = .task["formula"]
			args = c(list(f, data=get.data(.task, .subset), control=xs$control), xs$args)
      if (.task["has.weights"])
        args$weights = .task["weights"][.subset] 
			do.call(glmboost, args)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.glmboost", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			type = ifelse(.type=="response", "class", "response")
			p = predict(.model["learner.model"], newdata=.newdata, type=type, ...)
			if (.type == "prob") {
				p = p[,1]
				y = matrix(0, ncol=2, nrow=nrow(.newdata))
				colnames(y) <- .model["class.levels"]
				y[,1] = p
				y[,2] = 1-p
				return(y)
			} else {
				return(p)
			}
		}
)	





