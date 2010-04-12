#' @include wrapped.learner.r
#' @include wrapped.learner.classif.r
#' @include wrapped.learner.regr.r
#' @include resample.instance.r
#' @include train.learner.r
#' @include predict.learner.r

setClass(
		"tune.wrapper",
		contains = c("wrapped.learner"),
		representation = representation(
				base.learner = "wrapped.learner",
				method = "character",
				resampling = "resample.desc",
				control = "ANY",
				scale = "function"
		)
)

setClass(
		"tune.wrapper.classif",
		contains = c("tune.wrapper", "wrapped.learner.classif")
)

setClass(
		"tune.wrapper.regr",
		contains = c("tune.wrapper", "wrapped.learner.regr")
)


#' Constructor.
#' @title Constructor for tuning wrapper
setMethod(
		f = "initialize",
		signature = signature("tune.wrapper"),
		def = function(.Object, base.learner, resampling, method, control, scale) {
			if (missing(base.learner))
				return(.Object)
			bl = base.learner
			.Object@base.learner = bl
			.Object@method = method
			.Object@resampling = resampling
			.Object@control = control
			.Object@scale = scale
			callNextMethod(.Object, learner.name=paste("tuned",bl@learner.name,sep="-"), learner.pack="mlr", learner.props=bl@learner.props)
		}
)

#' @export 
make.tune.wrapper <- function(learner, resampling, method="grid", control, scale) {
	if (is.character(learner))
		learner = make.learner(learner)
	if (missing(scale))
		scale = identity
	if (is(learner, "wrapped.learner.classif"))
		tt = new("tune.wrapper.classif", base.learner=learner, resampling=resampling, method=method, control=control, scale=scale)
	else		
		tt = new("tune.wrapper.regr", base.learner=learner, resampling=resampling, method=method, control=control, scale=scale)
	return(tt)
}

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="tune.wrapper", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			wl = .wrapped.learner
			bl = wl@base.learner
			if (is(bl, "wrapped.learner.classif"))
				f = make.task
			else if (is(bl, "wrapped.learner.regr"))
				f = make.task
			
			lt = f(data=.data, target=.targetvar)	
			tr = tune(bl, task=lt, resampling=wl@resampling, method=wl@method, control=wl@control, model=TRUE, scale=wl@scale)
			m = tr$model["learner.model"]
			attr(m, "tuned.par") = tr$par
			attr(m, "tuned.perf") = tr$perf
			return(m)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "tune.wrapper", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			predict.learner(.wrapped.learner@base.learner, .wrapped.model, .newdata, .type, ...)
		}
)	

