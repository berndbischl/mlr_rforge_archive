# Kosten bei allen gecheckt.

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
		"classif.ada", 
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.ada"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = TRUE
			)
			
      par.descs = list(
        new("par.desc.disc", par.name="type", default="discrete", vals=c("discrete", "real", "gentle")),
        new("par.desc.double", par.name="iter", default=50L, lower=1L),
        new("par.desc.double", par.name="nu", default=0.1, lower=0),
        new("par.desc.double", par.name="bag.frac", default=0.5, lower=0, upper=1),
        new("par.desc.log", par.name="model.coef", default=TRUE),
        new("par.desc.log", par.name="bag.shift", default=FALSE),
        new("par.desc.double", par.name="max.iter", default=20L, lower=1L),
        new("par.desc.double", par.name="delta", default=1e-10, lower=0),
        new("par.desc.double", par.name="minsplit", default=20L, lower=1L),
        new("par.desc.double", par.name="minbucket", lower=1L),
        new("par.desc.double", par.name="cp", default=0.01, lower=0, upper=1),
        new("par.desc.double", par.name="maxcompete", default=4L, lower=0L, flags=list(optimize=FALSE)),
        new("par.desc.double", par.name="maxsurrogate", default=5L, lower=0L, flags=list(optimize=FALSE)),
        new("par.desc.disc", par.name="usesurrogate", default=2, vals=0:2),
        new("par.desc.disc", par.name="surrogatestyle", default=0, vals=0:1),
        # we use 30 as upper limit, see docs of rpart.control
        new("par.desc.double", par.name="maxdepth", default=30L, lower=1L, upper=30L)
      )
      
			callNextMethod(.Object, pack="ada", desc=desc, par.descs=par.descs)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.ada", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = .task["formula"]
			if (.task["has.costs"]) {
				lev = levels(task["data"][.subset, .vars][, .targetvar])
				.costs = .costs[lev, lev] 
				ada(f, data=get.data(.task, .subset, .vars), parms=list(loss=.costs), ...)
			} else
				ada(f, data=get.data(.task, .subset, .vars), ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.ada", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "vector", "prob")
			p = predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "prob")
				colnames(p) = .model["class.levels"] 
			return(p)
		}
)	



