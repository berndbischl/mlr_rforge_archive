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
        discrete.learner.parameter(name="type", default="discrete", vals=c("discrete", "real", "gentle")),
        integer.learner.parameter(name="iter", default=50L, lower=1L),
        numeric.learner.parameter(name="nu", default=0.1, lower=0),
        numeric.learner.parameter(name="bag.frac", default=0.5, lower=0, upper=1),
        logical.learner.parameter(name="model.coef", default=TRUE),
        logical.learner.parameter(name="bag.shift", default=FALSE),
        integer.learner.parameter(name="max.iter", default=20L, lower=1L),
        numeric.learner.parameter(name="delta", default=1e-10, lower=0),
        integer.learner.parameter(name="minsplit", default=20L, lower=1L),
        integer.learner.parameter(name="minbucket", lower=1L),
        numeric.learner.parameter(name="cp", default=0.01, lower=0, upper=1),
        integer.learner.parameter(name="maxcompete", default=4L, lower=0L, flags=list(optimize=FALSE)),
        integer.learner.parameter(name="maxsurrogate", default=5L, lower=0L, flags=list(optimize=FALSE)),
        discrete.learner.parameter(name="usesurrogate", default=2L, vals=0:2),
        discrete.learner.parameter(name="surrogatestyle", default=0L, vals=0:1),
        # we use 30 as upper limit, see docs of rpart.control
        integer.learner.parameter(name="maxdepth", default=30L, lower=1L, upper=30L)
      )
      
			callNextMethod(.Object, pack="ada", desc=desc, par.descs=par.descs)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.ada", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
      d = data=get.data(.task, .subset)
			if (.task["has.costs"]) {
			  cm = .task["costs"]
        # probably better to reorder the row/cols so they correspond with levels in d$target
        levs = levels(d[, .task["target"]]) 
        cm = cm[levs, levs]
				ada(f, data=d, parms=list(loss=cm), ...)
			} else
				ada(f, data=d, ...)
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



