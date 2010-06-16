#' @include base.wrapper.r

setClass(
		"multiclass.wrapper",
		contains = c("base.wrapper")
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("multiclass.wrapper"),
		def = function(.Object, learner, method) {
			.Object = set.hyper.pars(.Object, list(multiclass.method=method), type="multiclass")
			callNextMethod(.Object, learner)
		}
)


#' Getter.
#' @rdname multiclass.wrapper-class

setMethod(
		f = "[",
		signature = signature("multiclass.wrapper"),
		def = function(x,i,j,...,drop) {
			if (i == "supports.multiclass")
				return(TRUE)
			if (i == "supports.probs")
				return(FALSE)
			if (i == "supports.decision")
				return(FALSE)
			callNextMethod()
		}
)


#' @export
make.multiclass.wrapper = function(learner, method="one-vs-all") {
	if (method != "one-vs-all")
		stop("Only method one-vs-all is currently supported!")
	if (is.character(learner))
		learner = make.learner(learner)
	new("multiclass.wrapper", learner=learner, method=method)
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
			method = .learner["hyper.pars", type="multiclass"]$multiclass.method
			
			k = .data.desc["class.nr"]
			levs = .data.desc["class.levels"]
			y = .data[,.targetvar]
			models = list()
			args = list(...)
			args$multiclass.method=NULL
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
			method = .model["hyper.pars", type="multiclass"]$multiclass.method
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





