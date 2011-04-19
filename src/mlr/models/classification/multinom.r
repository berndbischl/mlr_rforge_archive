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
#' @include ClassifTask.R
roxygen()


setClass(
		"classif.multinom", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.multinom"),
		def = function(.Object) {
      par.set = makeParameterSet(
        makeLogicalLearnerParameter(id="Hess", default=FALSE),
        makeDiscreteLearnerParameter(id="summ", default=0L, vals=0:3),
        makeLogicalLearnerParameter(id="censored", default=FALSE),
        makeLogicalLearnerParameter(id="model", default=FALSE),
        makeIntegerLearnerParameter(id="maxit", default=100L, lower=1L),
        makeNumericLearnerParameter(id="rang", default=0.7),
        makeNumericLearnerParameter(id="decay", default=0),
        makeLogicalLearnerParameter(id="trace", default=TRUE),
        makeNumericLearnerParameter(id="abstoll", default=1.0e-4),
        makeNumericLearnerParameter(id="reltoll", default=1.0e-8)
      )
      
      .Object = callNextMethod(.Object, pack="nnet", par.set=par.set)
      
      #checked:
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = TRUE
      )
    }
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.multinom", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
      if (.task["has.weights"])
        multinom(f, data=getData(.task, .subset), weights=.task@weights[.subset], ...)
      else  
        multinom(f, data=getData(.task, .subset), ...)			
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.multinom", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "class", "probs")
			levs = getClassLevels(.model)
			p = predict(.model@learner.model, newdata=.newdata, type=.type, ...)
			if (.type == "probs" && length(levs)==2) {
				p = matrix(c(1-p, p), ncol=2, byrow=FALSE)
				colnames(p) = levs
			} 
			return(p)
		}
)	







