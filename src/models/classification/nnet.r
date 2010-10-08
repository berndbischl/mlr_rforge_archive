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
			
			desc = new("learner.desc.classif",
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					characters = FALSE,
					probs = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)

			par.descs = list(
        new("par.desc.num", par.name="size", default=3L, lower=0L, flags=list(pass.default=TRUE)),
        new("par.desc.num", par.name="maxit", default=100L, lower=1L),
        # nnet seems to set these manually and hard for classification.....
#        new("par.desc.log", par.new="linout", default=FALSE, requires=expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
#        new("par.desc.log", par.new="entropy", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
#        new("par.desc.log", par.new="softmax", default=FALSE, requires=expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
#        new("par.desc.log", par.new="censored", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
        new("par.desc.log", par.new="skip", default=FALSE),
        new("par.desc.num", par.new="rang", default=0.7),
        new("par.desc.num", par.new="decay", default=0),
        new("par.desc.log", par.new="Hess", default=FALSE, flags(optimze=FALSE)),
        new("par.desc.log", par.new="trace", default=TRUE, flags(optimze=FALSE)),
        new("par.desc.num", par.new="MaxNWts", default=1000L),
        new("par.desc.num", par.new="abstoll", default=1.0e-4),
        new("par.desc.num", par.new="reltoll", default=1.0e-8)
      )
      			
			callNextMethod(.Object, label="NNet", pack="nnet", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.nnet", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			nnet(f, data=.data, weights=.weights, ...)
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
