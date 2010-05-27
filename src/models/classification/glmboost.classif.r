# todo: we could pass costs with extra loss function?

#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"classif.glmboost", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.glmboost"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missings = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = TRUE,
					supports.costs = FALSE
			)
			callNextMethod(.Object, label="glmboost", pack="mboost", props=desc,
					parset.train=list(family = Binomial()))
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.glmboost", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
			xs = args.to.control(boost_control, c("mstop", "nu", "risk"), list(...))
			f = as.formula(paste(.targetvar, "~."))
			args = c(list(f, data=.data, weights=.weights, control=xs$control), xs$args)
			do.call(glmboost, args)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.learner = "classif.glmboost", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			type = ifelse(.type=="response", "class", "response")
			p = predict(.model["learner.model"], newdata=.newdata, type=type, ...)
			if (.type == "prob") {
				p = p[,1]
				y = matrix(0, ncol=2, nrow=nrow(.newdata))
				colnames(y) <- .model["class.levels"]
				y[,1] = p
				y[,2] = 1-p
				return(y)
			} else {
				return(p)
			}
		}
)	





