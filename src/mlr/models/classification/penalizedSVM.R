#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.r
roxygen()
#' @include predictLearner.r
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
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = FALSE,
					doubles = TRUE,
					factors = FALSE,
					prob = FALSE, 
					decision = FALSE,
					weights = FALSE,	
					costs = FALSE 
			)
			
      par.set = makeParameterSet(
        makeDiscreteLearnerParameter(id="fs.method", default="scad", vals=c("scad","1norm", "DrHSVM", "scad+L2")),
        makeNumericLearnerParameter(id="maxevals", default=500L),
        makeLogicalLearnerParameter(id="calc.class.weights", default=FALSE),
        makeNumericLearnerParameter(id="lambda1", lower=0),
        makeNumericLearnerParameter(id="lambda2", lower=0)
      )
      
			callNextMethod(.Object, pack="penalizedSVM", desc=desc, par.set=par.set)
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
			d = get.data(.task, .subset, target.extra=TRUE, class.as="-1+1")
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
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "response", "probabilities")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	


