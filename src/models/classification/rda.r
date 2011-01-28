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
		"classif.rda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.rda"),
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
					weights = FALSE,			
					costs = FALSE
			)
			par.set = list(
					numeric.learner.parameter(id="lambda", lower=0, upper=1),
					numeric.learner.parameter(id="gamma", lower=0, upper=1),
					logical.learner.parameter(id="crossval", default=TRUE),
          integer.learner.parameter(id="fold", default=10L, lower=1L),
					numeric.learner.parameter(id="train.fraction", default=0.5, lower=0, upper=1),
					logical.learner.parameter(id="crossval", default=TRUE),
					discrete.learner.parameter(id="schedule", default=1L, vals=1:2, requires=expression(simAnn==FALSE)),
					numeric.learner.parameter(id="T.start", default=0.1, lower=0, requires=expression(simAnn==TRUE)),
					numeric.learner.parameter(id="halflife", default=0.1, lower=0, requires=expression(simAnn==TRUE || schedule==1)),
					numeric.learner.parameter(id="zero.temp", default=0.01, lower=0, requires=expression(simAnn==TRUE || schedule==1)),
					numeric.learner.parameter(id="alpha", default=2, lower=1, requires=expression(simAnn==TRUE || schedule==2)),
          integer.learner.parameter(id="K", default=100L, lower=1L, requires=expression(simAnn==TRUE || schedule==2)),
					discrete.learner.parameter(id="kernel", default="triangular", 
							vals=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian")),
          logical.learner.parameter(id="trafo", default=TRUE),
          logical.learner.parameter(id="SimAnn", default=FALSE),
          # change default, so error is only estimated at request of user
          logical.learner.parameter(id="estimate.error", default=FALSE, flags=list(optimize=FALSE, pass.default=TRUE))
			)
			
			callNextMethod(.Object, pack="klaR", desc=desc, par.set=par.set)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.rda", 
				.task="classif.task", .subset="integer" 
		),
    # todo: disable crossval. no, is done automaticall if pars are set.
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			rda(f, data=get.data(.task, .subset), ...)
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
