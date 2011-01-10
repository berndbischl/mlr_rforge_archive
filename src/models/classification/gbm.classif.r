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
		"classif.gbm", 
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.gbm"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)			
      
      par.descs = list(      
          discrete.learner.parameter(name="distribution", default="bernoulli", vals=c("bernoulli", "adaboost")),
          integer.learner.parameter(name="n.trees", default=100L, lower=1L),
          integer.learner.parameter(name="interaction.depth", default=1L, lower=1L),
          integer.learner.parameter(name="n.minobsinnode", default=10L, lower=1L),
          numeric.learner.parameter(name="shrinkage", default=0.001, lower=0),
          numeric.learner.parameter(name="bag.fraction", default=0.5, lower=0, upper=1),
          numeric.learner.parameter(name="train.fraction", default=1, lower=0, upper=1)
      )
      callNextMethod(.Object, pack="gbm", desc=desc,	
          par.descs=par.descs, par.vals=list(distribution = "bernoulli"))
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.gbm", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			d = get.data(.task, .subset, class.as="01")
      if (.task["has.weights"])
        gbm(f, data=d, keep.data=FALSE, verbose=FALSE, weights=.task["weights"][.subset], ...)
      else  
        gbm(f, data=d, keep.data=FALSE, verbose=FALSE, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.gbm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			m = .model["learner.model"]
			p = predict(m, newdata=.newdata, type="response", n.trees=length(m$trees), single.tree=FALSE, ...)
			levs = c(.model["negative"], .model["positive"])
			if (.type == "prob") {
				y = matrix(0, ncol=2, nrow=nrow(.newdata))
				colnames(y) = levs
				y[,1] = 1-p
				y[,2] = p
				return(y)
			} else {
				p = as.factor(ifelse(p > 0.5, levs[2], levs[1]))
				names(p) = NULL
				return(p)
			}
		}
)	


