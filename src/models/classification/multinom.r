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
			
      
      par.set = list(
        logical.learner.parameter(id="Hess", default=FALSE, flags=list(optimize=FALSE)),
        discrete.learner.parameter(id="summ", default=0L, vals=0:3),
        logical.learner.parameter(id="censored", default=FALSE),
        logical.learner.parameter(id="model", default=FALSE),
        integer.learner.parameter(id="maxit", default=100L, lower=1L),
        numeric.learner.parameter(id="rang", default=0.7),
        numeric.learner.parameter(id="decay", default=0),
        logical.learner.parameter(id="trace", default=TRUE, flags=list(optimize=FALSE)),
        numeric.learner.parameter(id="abstoll", default=1.0e-4),
        numeric.learner.parameter(id="reltoll", default=1.0e-8)
      )
      
			callNextMethod(.Object, pack="nnet", desc=desc, par.set=par.set)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.multinom", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
      if (.task["has.weights"])
        multinom(f, data=get.data(.task, .subset), weights=.task["weights"][.subset], ...)
      else  
        multinom(f, data=get.data(.task, .subset), ...)			
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







