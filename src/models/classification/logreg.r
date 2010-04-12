#' @include wrapped.learner.classif.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include predict.learner.r
roxygen()


setClass(
		"classif.logreg", 
		contains = c("wrapped.learner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.logreg"),
		def = function(.Object, parset) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.decision = FALSE,
					supports.weights = TRUE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="logreg", learner.pack="stats", learner.props=desc, parset=parset)
		}
)

#' @rdname train.learner


setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="classif.logreg", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			glm(f, family="binomial", data=.data, model=FALSE, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "classif.logreg", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			
			x <- predict(.wrapped.model["learner.model"], newdata=.newdata, type="response", ...)
			
			if (.type == "prob") {
				y <- matrix(0, ncol=2, nrow=nrow(.newdata))
				colnames(y) <- .wrapped.model["class.levels"]
				y[,1] <- 1-x
				y[,2] <- x
				return(y)
			} else {
				levs <- .wrapped.model["class.levels"]
				p <- as.factor(ifelse(x > 0.5, levs[2], levs[1]))
				names(p) <- NULL
				return(p)
			}
		}
)	


