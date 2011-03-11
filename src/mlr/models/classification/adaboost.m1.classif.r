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
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = TRUE,
					numerics = TRUE,
					factors = TRUE,
					prob = FALSE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE
			)
      
      par.set = makeParameterSet(
        makeLogicalLearnerParameter(id="boos", default=TRUE),
        makeIntegerLearnerParameter(id="mfinal", default=100L, lower=1L),
        makeDiscreteLearnerParameter(id="coeflearn", default="Breiman", vals=c("Breiman", "Freund")),
        makeIntegerLearnerParameter(id="minsplit", default=5L, lower=1L),
        makeNumericLearnerParameter(id="cp", default=0.01, lower=0),
        makeIntegerLearnerParameter(id="maxdepth", lower=1L, upper=30L)
      )
			callNextMethod(.Object, pack="adabag", desc=desc, par.set=par.set)
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
			f = .task["formula"]
			adaboost.M1(f, data=get.data(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.adaboost.M1", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			# stupid adaboost
			.newdata[, .model@desc@target] <- factor(rep(1, nrow(.newdata)), levels=getClassLevels(.model))
			p = predict(.model["learner.model"], newdata=.newdata, ...)
			return(as.factor(p$class))
		}
)	




