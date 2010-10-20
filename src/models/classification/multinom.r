#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
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
        new("par.desc.log", par.name="Hess", default=FALSE, flags=list(optimize=FALSE)),
        new("par.desc.disc", par.name="summ", default=0L, vals=0:3),
        new("par.desc.log", par.name="censored", default=FALSE),
        new("par.desc.log", par.name="model", default=FALSE),
        new("par.desc.num", par.name="maxit", default=100L, lower=1L),
        new("par.desc.num", par.name="rang", default=0.7),
        new("par.desc.num", par.name="decay", default=0),
        new("par.desc.log", par.name="trace", default=TRUE, flagslist(optimze=FALSE)),
        new("par.desc.num", par.name="abstoll", default=1.0e-4),
        new("par.desc.num", par.name="reltoll", default=1.0e-8)
      )
      
			callNextMethod(.Object, label="MultiReg", pack="nnet", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.multinom", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			multinom(f, data=.data, weights=.weights, ...)
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







