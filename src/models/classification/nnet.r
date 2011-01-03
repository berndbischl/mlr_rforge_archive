#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
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
        new("par.desc.double", par.name="size", default=3L, lower=0L, flags=list(pass.default=TRUE)),
        new("par.desc.double", par.name="maxit", default=100L, lower=1L),
        # nnet seems to set these manually and hard for classification.....
#        new("par.desc.log", par.name="linout", default=FALSE, requires=expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
#        new("par.desc.log", par.name="entropy", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
#        new("par.desc.log", par.name="softmax", default=FALSE, requires=expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
#        new("par.desc.log", par.name="censored", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
        new("par.desc.log", par.name="skip", default=FALSE),
        new("par.desc.double", par.name="rang", default=0.7),
        new("par.desc.double", par.name="decay", default=0),
        new("par.desc.log", par.name="Hess", default=FALSE, flags=list(optimize=FALSE)),
        new("par.desc.log", par.name="trace", default=TRUE, flags=list(optimize=FALSE)),
        new("par.desc.double", par.name="MaxNWts", default=1000L),
        new("par.desc.double", par.name="abstoll", default=1.0e-4),
        new("par.desc.double", par.name="reltoll", default=1.0e-8)
      )
      			
			callNextMethod(.Object, pack="nnet", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.nnet", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = .task["formula"]
      if (.task["has.weights"])
        nnet(f, data=get.data(.task, .subset, .vars), weights=.task["weights"][.subset], ...)
      else  
        nnet(f, data=get.data(.task, .subset, .vars), ...)			
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
