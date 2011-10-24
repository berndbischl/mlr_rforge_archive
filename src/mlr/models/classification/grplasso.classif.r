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
		"classif.grplasso", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.grplasso"),
		def = function(.Object) {
      par.set = makeParameterSet(
        makeNumericLearnerParameter(id="lambda", default=1, lower=0),
        makeUntypedLearnerParameter(id="index")
      )
      
      .Object = callNextMethod(.Object, pack="grplasso", par.set=par.set, par.vals=list(lambda = 1))
      
      .Object = setProperties(.Object, 
        twoclass = TRUE,
        numerics = TRUE,
        prob = TRUE,
        weights = TRUE
      )
      
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.grplasso", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			# todo: bug in grplasso: index cant be passed with formula interface....
			d = getData(.task, .subset, target.extra=TRUE, class.as="01")
			x = cbind(1, as.matrix(d$data))
      if (.task@desc@has.weights)
			  grplasso(x, d$target, weights=.task@weights[.subset], ...)
      else
        grplasso(x, d$target, ...)
    }
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.grplasso", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			x = as.matrix(.newdata)
			x = cbind(1, x)
			p = as.numeric(predict(.model@learner.model, newdata=x, type="response", ...))
			levs = c(.model@task.desc@negative, .model@task.desc@positive) 		
			if (.learner@predict.type == "prob") {
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