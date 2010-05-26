#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"novars", 
		contains = c("base.wrapper")
)


setMethod(
		f = "initialize",
		signature = signature("novars"),
		def = function(.Object, learner) {
			callNextMethod(.Object, learner)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="novars", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="ANY" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
			list(targets=.data[, .targetvar])
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "novars", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "ANY" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			m = .model["learner.model"]
			# for regression return constant mean
			if (.learner["is.regr"])
				return(rep(mean(m$targets), nrow(.newdata)))
			tab <- prop.table(table(m$targets))
			probs <- as.numeric(tab) 
			if(.type=="response")
				return(sample(as.factor(names(tab)), nrow(.newdata), prob=probs, replace=TRUE))	
			else {
				probs = t(replicate(nrow(.newdata), probs))
				colnames(probs) = names(tab)
				return(probs)
			}
		}
)	







