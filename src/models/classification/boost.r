#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()

setClass(
		"classif.adaboost.M1", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.adaboost.M1"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missings = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = TRUE,
					supports.probs = FALSE,
					supports.decision = FALSE,
					supports.weights = FALSE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, label="AdaBoostM1", pack="adabag", props=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.adaboost.M1", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			adaboost.M1(f, data=.data, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "classif.adaboost.M1", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			# stupid adaboost
			.newdata[, .model["target"]] <- factor(rep(1, nrow(.newdata)), levels=.model["class.levels"])
			p = predict(.model["learner.model"], newdata=.newdata, ...)
			return(as.factor(p$class))
		}
)	




