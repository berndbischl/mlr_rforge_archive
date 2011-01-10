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
		"classif.grplasso", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.grplasso"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = FALSE,
					doubles = TRUE,
					factors = FALSE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)

      par.descs = list(
        numeric.learner.parameter(name="lambda", default=1, lower=0),
        new("par.desc.unknown", name="index")
      )
      
      callNextMethod(.Object, pack="grplasso", desc=desc, par.descs=par.descs, par.vals=list(lambda = 1))
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.grplasso", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			# todo: bug in grplasso: index cant be passed with formula interface....
			d = get.data(.task, .subset, target.extra=TRUE, class.as="01")
			x = cbind(1, as.matrix(d$data))
      if (.task["has.weights"])
			  grplasso(x, d$target, weights=.task["weights"][.subset], ...)
      else
        grplasso(x, d$target, ...)
    }
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.grplasso", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			x = as.matrix(.newdata)
			x = cbind(1, x)
			p = as.numeric(predict(.model["learner.model"], newdata=x, type="response", ...))
			levs = c(.model["negative"], .model["positive"]) 		
			if (.type == "prob") {
				y <- matrix(0, ncol=2, nrow=nrow(.newdata))
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