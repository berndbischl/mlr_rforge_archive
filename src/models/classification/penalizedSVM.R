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
			
      par.descs = list(
        discrete.learner.parameter(name="fs.method", default="scad", vals=c("scad","1norm", "DrHSVM", "scad+L2")),
        numeric.learner.parameter(name="maxevals", default=500L),
        logical.learner.parameter(name="calc.class.weights", default=FALSE),
        numeric.learner.parameter(name="lambda1", lower=0),
        numeric.learner.parameter(name="lambda2", lower=0)
      )
      
			callNextMethod(.Object, pack="penalizedSVM", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.lpsvm", 
				.task="classif.task", .subset="integer" 
		),
		def = function(.learner, .task, .subset,  ...) {
			d = get.data(.task, .subset, target.extra=TRUE, class.as="-1+1")
			svm.fs(x=as.matrix(d$data), y=d$target, verbose=FALSE, grid.search="discrete", parms.coding="none",
        lambda1.set=2, lambda2.set=2, inner.val.method="cv", cross.inner=2,
        set.seed=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.lpsvm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "response", "probabilities")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	


