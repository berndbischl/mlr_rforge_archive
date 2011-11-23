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
		"classif.rda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.rda"),
		def = function(.Object) {
			par.set = makeParamSet(
					makeNumericLearnerParam(id="lambda", lower=0, upper=1),
					makeNumericLearnerParam(id="gamma", lower=0, upper=1),
					makeLogicalLearnerParam(id="crossval", default=TRUE),
          makeIntegerLearnerParam(id="fold", default=10L, lower=1L),
					makeNumericLearnerParam(id="train.fraction", default=0.5, lower=0, upper=1),
					makeDiscreteLearnerParam(id="schedule", default=1L, vals=1:2, requires=expression(simAnn==FALSE)),
					makeNumericLearnerParam(id="T.start", default=0.1, lower=0, requires=expression(simAnn==TRUE)),
					makeNumericLearnerParam(id="halflife", default=0.1, lower=0, requires=expression(simAnn==TRUE || schedule==1)),
					makeNumericLearnerParam(id="zero.temp", default=0.01, lower=0, requires=expression(simAnn==TRUE || schedule==1)),
					makeNumericLearnerParam(id="alpha", default=2, lower=1, requires=expression(simAnn==TRUE || schedule==2)),
          makeIntegerLearnerParam(id="K", default=100L, lower=1L, requires=expression(simAnn==TRUE || schedule==2)),
					makeDiscreteLearnerParam(id="kernel", default="triangular", 
							vals=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian")),
          makeLogicalLearnerParam(id="trafo", default=TRUE),
          makeLogicalLearnerParam(id="SimAnn", default=FALSE),
          # change default, so error is only estimated at request of user
          makeLogicalLearnerParam(id="estimate.error", default=FALSE, pass.default=TRUE)
			)
			
			.Object = callNextMethod(.Object, pack="klaR", par.set=par.set)
    
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE
      )
    }
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.rda", 
				.task="ClassifTask", .subset="integer" 
		),
    # todo: disable crossval. no, is done automaticall if pars are set.
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			rda(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.rda", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			p <- predict(.model@learner.model, newdata=.newdata, ...)
			if (.learner@predict.type == "response")
				return(p$class)
			else
				return(p$posterior)
			
		}
)	
