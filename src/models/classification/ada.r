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
			
      par.set = list(
        makeDiscreteLearnerParameter(id="type", default="discrete", vals=c("discrete", "real", "gentle")),
        makeIntegerLearnerParameter(id="iter", default=50L, lower=1L),
        makeNumericLearnerParameter(id="nu", default=0.1, lower=0),
        makeNumericLearnerParameter(id="bag.frac", default=0.5, lower=0, upper=1),
        makeLogicalLearnerParameter(id="model.coef", default=TRUE),
        makeLogicalLearnerParameter(id="bag.shift", default=FALSE),
        makeIntegerLearnerParameter(id="max.iter", default=20L, lower=1L),
        makeNumericLearnerParameter(id="delta", default=1e-10, lower=0),
        makeIntegerLearnerParameter(id="minsplit", default=20L, lower=1L),
        makeIntegerLearnerParameter(id="minbucket", lower=1L),
        makeNumericLearnerParameter(id="cp", default=0.01, lower=0, upper=1),
        makeIntegerLearnerParameter(id="maxcompete", default=4L, lower=0L, flags=list(optimize=FALSE)),
        makeIntegerLearnerParameter(id="maxsurrogate", default=5L, lower=0L, flags=list(optimize=FALSE)),
        makeDiscreteLearnerParameter(id="usesurrogate", default=2L, vals=0:2),
        makeDiscreteLearnerParameter(id="surrogatestyle", default=0L, vals=0:1),
        # we use 30 as upper limit, see docs of rpart.control
        makeIntegerLearnerParameter(id="maxdepth", default=30L, lower=1L, upper=30L)
      )
      
			callNextMethod(.Object, pack="ada", desc=desc, par.set=par.set)
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



