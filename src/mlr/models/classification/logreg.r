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
		"classif.logreg", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.logreg"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)
			
			callNextMethod(.Object, pack="stats", desc=desc)
		}
)

#' @rdname trainLearner


setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.logreg", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			glm(f, data=get.data(.task, .subset), model=FALSE, family="binomial", ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.logreg", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			
			x = predict(.model["learner.model"], newdata=.newdata, type="response", ...)
			levs = .model["class.levels"]		
			if (.type == "prob") {
				y <- matrix(0, ncol=2, nrow=nrow(.newdata))
				colnames(y) = levs
				y[,1] <- 1-x
				y[,2] <- x
				return(y)
			} else {
				levs <- .model["class.levels"]
				p <- as.factor(ifelse(x > 0.5, levs[2], levs[1]))
				names(p) <- NULL
				return(p)
			}
		}
)	


