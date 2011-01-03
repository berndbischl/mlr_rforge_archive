#' @include learnerR.r
roxygen()
#' @include task.regr.r
roxygen()

setClass(
		"regr.gbm", 
		contains = c("rlearner.regr")
)


	

setMethod(
		f = "initialize",
		signature = signature("regr.gbm"),
		def = function(.Object) {
			
			desc = c(
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					weights = TRUE
			)
			
      par.descs = list(      
          new("par.desc.disc", par.name="distribution", default="gaussian", vals=c("gaussian", "laplace")),
          new("par.desc.double", par.name="n.trees", default=100L, lower=1L),
          new("par.desc.double", par.name="interaction.depth", default=1L, lower=1L),
          new("par.desc.double", par.name="n.minobsinnode", default=10L, lower=1L),
          new("par.desc.double", par.name="shrinkage", default=0.001, lower=0),
          new("par.desc.double", par.name="bag.fraction", default=0.5, lower=0, upper=1),
          new("par.desc.double", par.name="train.fraction", default=1, lower=0, upper=1)
      )
      
			callNextMethod(.Object, pack="gbm", desc=desc, par.descs=par.descs, par.vals=list(distribution = "gaussian"))
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.gbm", 
				.task="regr.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
      if (.task["has.weights"])
        gbm(f, data=get.data(.task, .subset), keep.data=FALSE, weights=.task["weights"][.subset], ...)
      else  
        gbm(f, data=get.data(.task, .subset), keep.data=FALSE, ...)
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







