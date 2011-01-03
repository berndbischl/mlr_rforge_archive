#' @include learnerR.r
roxygen()


setClass(
		"regr.gbm", 
		contains = c("rlearner.regr")
)


	

setMethod(
		f = "initialize",
		signature = signature("regr.gbm"),
		def = function(.Object) {
			
			desc = new("learner.desc.regr",
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					weights = TRUE
			)
			
      par.descs = list(      
          new("par.desc.disc", par.name="distribution", default="gaussian", vals=c("gaussian", "laplace")),
          new("par.desc.num", par.name="n.trees", default=100L, lower=1L),
          new("par.desc.num", par.name="interaction.depth", default=1L, lower=1L),
          new("par.desc.num", par.name="n.minobsinnode", default=10L, lower=1L),
          new("par.desc.num", par.name="shrinkage", default=0.001, lower=0),
          new("par.desc.num", par.name="bag.fraction", default=0.5, lower=0, upper=1),
          new("par.desc.num", par.name="train.fraction", default=1, lower=0, upper=1)
      )
      
			callNextMethod(.Object, pack="gbm", desc=desc, par.vals=list(distribution = "gaussian"))
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.gbm", 
				.task="regr.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = as.formula(paste(.task["target"], "~."))
			gbm(f, data=.task["data"][.subset, .vars], weights=.weights, keeptask["data"][.subset, .vars]=FALSE, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "regr.gbm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			m <- .model["learner.model"]
			predict(m, newdata=.newdata, n.trees=length(m$trees), ...)
		}
)	







