#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()


setClass(
		"classif.svm", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.svm"),
		def = function(.Object) {
			
			desc = new("learner.desc.classif",
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = TRUE,
					numerics = TRUE,
					factors = TRUE,
					characters = TRUE,
					probs = TRUE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE 
			)
			
			callNextMethod(.Object, label="SVM", pack="e1071", desc=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.svm", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			svm(f, data=.data, probability=TRUE, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.svm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			if(.type=="response") {
				p = predict(.model["learner.model"], newdata=.newdata, ...)
			} else {
				p = predict(.model["learner.model"], newdata=.newdata, probability=TRUE, ...)
				p = attr(p, "probabilities")
			}
			return(p)
		}
)	


