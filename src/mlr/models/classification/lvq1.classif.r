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
		"classif.lvq1", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.lvq1"),
		def = function(.Object) {
			.Object = callNextMethod(.Object, pack="class")

      setProperties(.Object, 
        oneclass = FALSE,
        twoclass = TRUE,
        multiclass = TRUE,
        missings = FALSE,
        numerics = TRUE,
        factors = TRUE,
        prob = FALSE,
        weights = FALSE
      )
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.lvq1", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
      d = getData(.task, .subset, target.extra=TRUE)
			cdbk.args = insert(list(), list(...), c("size", "k", "prior"))
			cdbk.args$x = d$data
			cdbk.args$cl = d$target 
			codebk = do.call(lvqinit, cdbk.args)  

			lvq.args = insert(list(), list(...), c("niter", "alpha"))
			lvq.args$x = d$data 
			lvq.args$cl = d$target 
			lvq.args$codebk = codebk 
			do.call(lvq1, lvq.args)  
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.lvq1", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			lvqtest(.model@learner.model, test=.newdata, ...)
		}
)	






