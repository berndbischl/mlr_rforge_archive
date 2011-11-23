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
		"classif.lpsvm", 
		contains = c("rlearner.classif")
)

setMethod(
		f = "initialize",
		signature = signature("classif.lpsvm"),
		def = function(.Object) {
      par.set = makeParamSet(
        makeDiscreteLearnerParam(id="fs.method", default="scad", vals=c("scad","1norm", "DrHSVM", "scad+L2")),
        makeNumericLearnerParam(id="maxevals", default=500L),
        makeLogicalLearnerParam(id="calc.class.weights", default=FALSE),
        makeNumericLearnerParam(id="lambda1", lower=0),
        makeNumericLearnerParam(id="lambda2", lower=0)
      )
      
			.Object = callNextMethod(.Object, pack="penalizedSVM", par.set=par.set)
      
      setProperties(.Object, 
        twoclass = TRUE,
        numerics = TRUE
      )
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.lpsvm", 
				.task="ClassifTask", .subset="integer" 
		),
		def = function(.learner, .task, .subset,  ...) {
			d = getData(.task, .subset, target.extra=TRUE, class.as="-1+1")
			svm.fs(x=as.matrix(d$data), y=d$target, verbose=FALSE, grid.search="discrete", parms.coding="none",
        lambda1.set=2, lambda2.set=2, inner.val.method="cv", cross.inner=2,
        set.seed=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.lpsvm", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = ifelse(.learner@predict.type == "response", "response", "probabilities")
			predict(.model@learner.model, newdata=.newdata, type=type, ...)
		}
)	


