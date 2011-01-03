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
#' @include task.classif.r
roxygen()


setClass(
		"classif.multinom", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.multinom"),
		def = function(.Object) {
			
			#checked:
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
        new("par.desc.log", par.name="Hess", default=FALSE, flags=list(optimize=FALSE)),
        new("par.desc.disc", par.name="summ", default=0L, vals=0:3),
        new("par.desc.log", par.name="censored", default=FALSE),
        new("par.desc.log", par.name="model", default=FALSE),
        new("par.desc.double", par.name="maxit", default=100L, lower=1L),
        new("par.desc.double", par.name="rang", default=0.7),
        new("par.desc.double", par.name="decay", default=0),
        new("par.desc.log", par.name="trace", default=TRUE, flags=list(optimize=FALSE)),
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
				.learner="classif.multinom", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = .task["formula"]
      if (.task["has.weights"])
        multinom(f, data=get.data(.task, .subset, .vars), weights=.task["weights"][.subset], ...)
      else  
        multinom(f, data=get.data(.task, .subset, .vars), ...)			
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.multinom", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "class", "probs")
			levs = .model["class.levels"]
			p = predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "probs" && length(levs)==2) {
				p = matrix(c(1-p, p), ncol=2, byrow=FALSE)
				colnames(p) = levs
			} 
			return(p)
		}
)	







