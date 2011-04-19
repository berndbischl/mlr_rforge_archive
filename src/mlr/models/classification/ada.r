# Kosten bei allen gecheckt.

#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include ClassifTask.R
roxygen()


setClass(
		"classif.ada", 
		contains = c("rlearner.classif")
)



setMethod(
		f = "initialize",
		signature = signature("classif.ada"),
		def = function(.Object) {
						
      par.set = makeParameterSet(
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
        makeIntegerLearnerParameter(id="maxcompete", default=4L, lower=0L),
        makeIntegerLearnerParameter(id="maxsurrogate", default=5L, lower=0L),
        makeDiscreteLearnerParameter(id="usesurrogate", default=2L, vals=0:2),
        makeDiscreteLearnerParameter(id="surrogatestyle", default=0L, vals=0:1),
        # we use 30 as upper limit, see docs of rpart.control
        makeIntegerLearnerParameter(id="maxdepth", default=30L, lower=1L, upper=30L)
      )
      
			.Object = callNextMethod(.Object, pack="ada", par.set=par.set)
      
      setProperties(.Object, 
        twoclass = TRUE,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = TRUE,
        costs = TRUE
      )
		}
)


#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.ada", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
      d = data=getData(.task, .subset)
#			if (.task["has.costs"]) {
#			  cm = .task["costs"]
#        # probably better to reorder the row/cols so they correspond with levels in d$target
#        levs = levels(d[, .task@desc@target]) 
#        cm = cm[levs, levs]
#				ada(f, data=d, parms=list(loss=cm), ...)
#			} else
				ada(f, data=d, ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.ada", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "vector", "prob")
			p = predict(.model@learner.model, newdata=.newdata, type=.type, ...)
			if (.type == "prob")
				colnames(p) = getClassLevels(.model) 
			return(p)
		}
)	



