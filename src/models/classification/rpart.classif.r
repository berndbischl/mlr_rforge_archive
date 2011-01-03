#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()


# todo: parms has to be in hyperparamter list

setClass(
		"classif.rpart", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.rpart"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = TRUE
			)
			par.descs = list(
					new("par.desc.double", par.name="minsplit", default=20L, lower=1L),
					new("par.desc.double", par.name="minbucket", lower=1L),
					new("par.desc.double", par.name="cp", default=0.01, lower=0, upper=1),
					new("par.desc.double", par.name="maxcompete", default=4L, lower=0L, flags=list(optimize=FALSE)),
					new("par.desc.double", par.name="maxsurrogate", default=5L, lower=0L, flags=list(optimize=FALSE)),
					new("par.desc.disc", par.name="usesurrogate", default=2L, vals=0:2),
					new("par.desc.disc", par.name="surrogatestyle", default=0L, vals=0:1),
          # we use 30 as upper limit, see docs of rpart.control
					new("par.desc.double", par.name="maxdepth", default=30L, lower=1L, upper=30L)
			)
			
			callNextMethod(.Object, pack="rpart", desc=desc, par.descs=par.descs)
		}
)

#' @rdname train.learner


setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.rpart", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = .task["formula"]
			if (.task["has.costs"]) {
				lev = levels(.task["data"][.subset, .vars][, .targetvar])
				.costs = .costs[lev, lev] 
				rpart(f, data=get.data(.task, .subset, .vars), weights=.weights, parms=list(loss=.costs), ...)
			} else
				rpart(f, data=get.data(.task, .subset, .vars), weights=.weights, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.rpart", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type = switch(.type, prob="prob", "class")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	





