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
		"classif.adaboost.M1", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.adaboost.M1"),
		def = function(.Object) {
			
      
      par.set = makeParameterSet(
        makeLogicalLearnerParameter(id="boos", default=TRUE),
        makeIntegerLearnerParameter(id="mfinal", default=100L, lower=1L),
        makeDiscreteLearnerParameter(id="coeflearn", default="Breiman", vals=c("Breiman", "Freund")),
        # rpart.control arguments
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
		
      .Object = callNextMethod(.Object, pack="adabag", par.set=par.set)
      
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE
      )
    }
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.adaboost.M1", 
				.task="ClassifTask", .subset="integer" 
		),
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
      xs = learnerArgsToControl(rpart.control, c("minsplit", "minbucket", "cp", "maxcompete", "maxsurrogate", "usesurrogate", "surrogatestyle", "maxdepth"), list(...))
      do.call(adaboost.M1, c(list(f, data=getData(.task, .subset), control=xs$control), xs$args))
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.adaboost.M1", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			# stupid adaboost
			.newdata[, .model@task.desc@target] <- factor(rep(1, nrow(.newdata)), levels=.model@task.desc@class.levels)
			p = predict(.model@learner.model, newdata=.newdata, ...)
			return(as.factor(p$class))
		}
)	




