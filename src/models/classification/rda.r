#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()

setClass(
		"classif.rda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.rda"),
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
					weights = FALSE,			
					costs = FALSE
			)
#			par.descs = list(
#					new("par.desc.num", par.name="lambda", data.type="numeric", default=as.numeric(NA), when="train", lower=0, upper=1),
#					new("par.desc.num", par.name="gamma ", data.type="numeric", default=as.numeric(NA), when="train", lower=0, upper=1),
#					new("par.desc.log", par.name="crossval", data.type="logical", default=TRUE, when="train"),
#					new("par.desc.num", par.name="fold", data.type="integer", default=10, when="train", lower=1, upper=Inf),
#					new("par.desc.num", par.name="train.fraction", data.type="numeric", default=0.5, when="train", lower=0, upper=1),
#					new("par.desc.log", par.name="crossval", data.type="logical", default=TRUE, when="train"),
#					new("par.desc.disc", par.name="schedule", data.type="integer", default=1, when="train", vals=1:2, requires=quote(simAnn==FALSE)),
#					new("par.desc.num", par.name="T.start", data.type="numeric", default=0.1, when="train", lower=0, upper=Inf, requires=quote(simAnn==TRUE)),
#					new("par.desc.num", par.name="halflife", data.type="integer", default=0.1, when="train", lower=0, upper=Inf, requires=quote(simAnn==TRUE || schedule==1)),
#					new("par.desc.num", par.name="zero.temp", data.type="numeric", default=0.01, when="train", lower=0, upper=Inf, requires=quote(simAnn==TRUE || schedule==1)),
#					new("par.desc.num", par.name="alpha", data.type="integer", default=2, when="train", lower=1, upper=Inf, requires=quote(simAnn==TRUE || schedule==2)),
#					new("par.desc.num", par.name="K", data.type="integer", default=100, when="train", lower=1, upper=Inf, requires=quote(simAnn==TRUE || schedule==2)),
#					
#					new("par.desc.disc", par.name="kernel", data.type="character", default="triangular", when="train", 
#							vals=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian"))
#			)
			
			callNextMethod(.Object, label="rda", pack="klaR", desc=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.rda", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),

		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			rda(f, data=.data, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.rda", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			p <- predict(.model["learner.model"], newdata=.newdata, ...)
			if (.type=="response")
				return(p$class)
			else
				return(p$posterior)
			
		}
)	
