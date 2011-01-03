#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()



setClass(
		"classif.lpsvm", 
		contains = c("rlearner.classif")
)

setMethod(
		f = "initialize",
		signature = signature("classif.lpsvm"),
		def = function(.Object) {
			
			desc = new("learner.desc.classif",
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					probs = TRUE, 
					decision = FALSE,
					weights = TRUE,	
					costs = FALSE 
			)
			
      par.descs = list(
        new("par.desc.num", par.name="maxevals", default=500L),
        new("par.desc.log", par.name="calc.class.weights", default=FALSE),
        new("par.desc.log", par.name="verbose", default=TRUE),
        new("par.des.num", par.name="seed", default=123),
        new("par.des.num", par.name="maxIter", default=700L),
        new("par.desc.num", par.name="k", default=5L),
        new("par.desc.num", par.name="nu", defaul=0),
        new("par.desc.log", par.name="output", default=0L, lower=0L, upper=1L),
        new("par.desc.num", par.name="delta", default=0.001),
        new("par.des.num", par.name="epsi", default=0.0001)
      )
      
			callNextMethod(.Object, pack="penalizedSVM", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.lpsvm", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			kpar = list()
			args = list(...)
			args.names <- names(args)
		
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


