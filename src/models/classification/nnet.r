#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()


setClass(
		"classif.nnet", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.nnet"),
		def = function(.Object) {
			
			desc = new("learner.desc.classif",
					multiclass = TRUE,
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					characters = FALSE,
					probs = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = FALSE
			)
			
			callNextMethod(.Object, label="NNet", pack="nnet", desc=desc, parset.train=list(size=1))
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.nnet", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			nnet(f, data=.data, weights=.weights, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.nnet", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type = switch(.type, response="class", prob="raw")
			p = predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "class")
				return(as.factor(p))
			else {
				if (.model["class.nr"] == 2) {
					y = matrix(0, ncol=2, nrow=nrow(.newdata))
					colnames(y) = .model["class.levels"]
					y[,1] <- p
					y[,2] <- 1-p
					return(y)
				} else
					return(p)	
			}
		}
)	






