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
		"classif.nnet", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.nnet"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)

			par.descs = list(
        integer.learner.parameter(name="size", default=3L, lower=0L, flags=list(pass.default=TRUE)),
        integer.learner.parameter(name="maxit", default=100L, lower=1L),
        # nnet seems to set these manually and hard for classification.....
#        logical.learner.parameter(name="linout", default=FALSE, requires=expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
#        logical.learner.parameter(name="entropy", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
#        logical.learner.parameter(name="softmax", default=FALSE, requires=expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
#        logical.learner.parameter(name="censored", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
        logical.learner.parameter(name="skip", default=FALSE),
        numeric.learner.parameter(name="rang", default=0.7),
        numeric.learner.parameter(name="decay", default=0),
        logical.learner.parameter(name="Hess", default=FALSE, flags=list(optimize=FALSE)),
        logical.learner.parameter(name="trace", default=TRUE, flags=list(optimize=FALSE)),
        integer.learner.parameter(name="MaxNWts", default=1000L),
        numeric.learner.parameter(name="abstoll", default=1.0e-4),
        numeric.learner.parameter(name="reltoll", default=1.0e-8)
      )
      			
			callNextMethod(.Object, pack="nnet", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.nnet", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
      if (.task["has.weights"])
        nnet(f, data=get.data(.task, .subset), weights=.task["weights"][.subset], ...)
      else  
        nnet(f, data=get.data(.task, .subset), ...)			
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.nnet", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type = switch(.type, response="class", prob="raw")
			p = predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "class")
				return(as.factor(p))
			else {
				if (.model["class.nr"] == 2) {
          y <- cbind(p, 1-p) 
					colnames(y) = .model["class.levels"]
					return(y)
				} else
					return(p)	
			}
		}
)	
