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
        new("par.desc.double", par.name="maxevals", default=500L),
        new("par.desc.log", par.name="calc.class.weights", default=FALSE),
        new("par.desc.log", par.name="verbose", default=TRUE),
        new("par.desc.double", par.name="seed", default=123),
        new("par.desc.double", par.name="maxIter", default=700L),
        new("par.desc.double", par.name="k", default=5L),
        new("par.desc.double", par.name="nu", default=0, lower=0),
        new("par.desc.double", par.name="delta", default=0.001, lower=0),
        new("par.desc.double", par.name="epsi", default=0.0001, lower=0)
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
			lpsvm(A=d$data, y=d$target, ...)
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


