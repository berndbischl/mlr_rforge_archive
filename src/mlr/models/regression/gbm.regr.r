#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
		"regr.gbm", 
		contains = c("rlearner.regr")
)


	

setMethod(
		f = "initialize",
		signature = signature("regr.gbm"),
		def = function(.Object) {
      par.set = makeParameterSet(      
          makeDiscreteLearnerParameter(id="distribution", default="gaussian", vals=c("gaussian", "laplace")),
          makeIntegerLearnerParameter(id="n.trees", default=100L, lower=1L),
          makeIntegerLearnerParameter(id="interaction.depth", default=1L, lower=1L),
          makeIntegerLearnerParameter(id="n.minobsinnode", default=10L, lower=1L),
          makeNumericLearnerParameter(id="shrinkage", default=0.001, lower=0),
          makeNumericLearnerParameter(id="bag.fraction", default=0.5, lower=0, upper=1),
          makeNumericLearnerParameter(id="train.fraction", default=1, lower=0, upper=1)
      )
      
			.Object = callNextMethod(.Object, pack="gbm", par.set=par.set, par.vals=list(distribution = "gaussian"))
      
      setProperties(.Object,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
        weights = TRUE
      )
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.gbm", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
      if (.task["has.weights"])
        gbm(f, data=getData(.task, .subset), keep.data=FALSE, weights=.task@weights[.subset], ...)
      else  
        gbm(f, data=getData(.task, .subset), keep.data=FALSE, ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.gbm", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			m <- .model@learner.model
			predict(m, newdata=.newdata, n.trees=length(m$trees), ...)
		}
)	







