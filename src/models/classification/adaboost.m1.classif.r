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
					doubles = TRUE,
					factors = TRUE,
					prob = FALSE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE
			)
      
      par.set = list(
        logical.learner.parameter(id="boos", default=TRUE),
        integer.learner.parameter(id="mfinal", default=100L, lower=1L),
        discrete.learner.parameter(id="coeflearn", default="Breiman", vals=c("Breiman", "Freund")),
        integer.learner.parameter(id="minsplit", default=5L, lower=1L),
        numeric.learner.parameter(id="cp", default=0.01, lower=0),
        integer.learner.parameter(id="maxdepth", lower=1L, upper=30L)
      )
			callNextMethod(.Object, pack="adabag", desc=desc, par.set=par.set)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.adaboost.M1", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			adaboost.M1(f, data=get.data(.task, .subset), ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
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




