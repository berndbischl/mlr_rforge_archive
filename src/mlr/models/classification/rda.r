#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include ClassifTask.R
roxygen()

setClass(
		"classif.rda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.rda"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = FALSE,			
					costs = FALSE
			)
			par.set = makeParameterSet(
					makeNumericLearnerParameter(id="lambda", lower=0, upper=1),
					makeNumericLearnerParameter(id="gamma", lower=0, upper=1),
					makeLogicalLearnerParameter(id="crossval", default=TRUE),
          makeIntegerLearnerParameter(id="fold", default=10L, lower=1L),
					makeNumericLearnerParameter(id="train.fraction", default=0.5, lower=0, upper=1),
					makeDiscreteLearnerParameter(id="schedule", default=1L, vals=1:2, requires=expression(simAnn==FALSE)),
					makeNumericLearnerParameter(id="T.start", default=0.1, lower=0, requires=expression(simAnn==TRUE)),
					makeNumericLearnerParameter(id="halflife", default=0.1, lower=0, requires=expression(simAnn==TRUE || schedule==1)),
					makeNumericLearnerParameter(id="zero.temp", default=0.01, lower=0, requires=expression(simAnn==TRUE || schedule==1)),
					makeNumericLearnerParameter(id="alpha", default=2, lower=1, requires=expression(simAnn==TRUE || schedule==2)),
          makeIntegerLearnerParameter(id="K", default=100L, lower=1L, requires=expression(simAnn==TRUE || schedule==2)),
					makeDiscreteLearnerParameter(id="kernel", default="triangular", 
							vals=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian")),
          makeLogicalLearnerParameter(id="trafo", default=TRUE),
          makeLogicalLearnerParameter(id="SimAnn", default=FALSE),
          # change default, so error is only estimated at request of user
          makeLogicalLearnerParameter(id="estimate.error", default=FALSE, flags=list(optimize=FALSE, pass.default=TRUE))
			)
			
			callNextMethod(.Object, pack="klaR", desc=desc, par.set=par.set)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.rda", 
				.task="ClassifTask", .subset="integer" 
		),
    # todo: disable crossval. no, is done automaticall if pars are set.
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			rda(f, data=get.data(.task, .subset), ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.rda", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			p <- predict(.model["learner.model"], newdata=.newdata, ...)
			if (.type=="response")
				return(p$class)
			else
				return(p$posterior)
			
		}
)	
