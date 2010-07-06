#' @include base.wrapper.r
roxygen()


#' Wrapper class for learners to handle multi-class problems. 
#' 
#' @exportClass multiclass.wrapper
#' @title Wrapper class for learners to handle multi-class problems.
setClass(
		"multiclass.wrapper",
		contains = c("base.wrapper")
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("multiclass.wrapper"),
		def = function(.Object, learner) {
			callNextMethod(.Object, learner)
		}
)

#' @rdname multiclass.wrapper-class

setMethod(
		f = "[",
		signature = signature("multiclass.wrapper"),
		def = function(x,i,j,...,drop) {
			if (i == "multiclass")
				return(TRUE)
			if (i == "probs")
				return(FALSE)
			if (i == "decision")
				return(FALSE)
			callNextMethod()
		}
)

#' Fuses a base learner with a multi-class method. Creates a learner object, which can be
#' used like any other learner object. This way learners which can only handle binary classification 
#' will be able to handle multi-class problems too.
#' Currently only "one-vs-all" is implemented.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param ... [any] \cr
#'        Optional parameters. Not used currently.   
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with multiclass method.
#' @export
make.multiclass.wrapper = function(learner, ...) {
	if (is.character(learner))
		learner = make.learner(learner)
	new("multiclass.wrapper", learner=learner)
}


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="multiclass.wrapper", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="matrix" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {	
			k = .data.desc["class.nr"]
			levs = .data.desc["class.levels"]
			y = .data[,.targetvar]
			models = list()
			args = list(...)
			for (i in 1:k) {
				cl = levs[i]
				.data[, .targetvar] = as.factor(y == cl)
				ct = make.task(data=.data, target=.targetvar, positive="TRUE")
				m = train(.learner["learner"], ct, parset=args)
				models[[i]] = m 
			}
			names(models) = levs
			return(models)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "multiclass.wrapper", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			models = .model["learner.model"]
			
			k = length(models)
			p = matrix(0, nrow(.newdata), ncol=k)
			levs = names(models)
			for (i in 1:k) {
				m = models[[i]]
				p[,i] = predict(m, newdata=.newdata, type="prob", ...)["prob"]
			}
			as.factor(apply(p, 1, function(x) vote.max.val(x, levs)))
		}
)	





