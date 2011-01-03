#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
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
          new("par.desc.num", par.name="lambda", default=1, lower=0)
      )
      
			callNextMethod(.Object, pack="grplasso", desc=desc, par.vals=list(lambda = 1))
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.grplasso", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = as.formula(paste(.task["target"], "~."))
			pos = .task.desc["positive"]
			# todo: bug in grplasso: index cant be passed with formula interface....
			y = as.numeric(task["data"][.subset, .vars][,.targetvar] == pos) 
			x = as.matrix(task["data"][.subset, .vars][, !(colnames(task["data"][.subset, .vars]) == .targetvar)])
			x = cbind(1, x)
			grplasso(x, y, weights=.weights, ...)
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