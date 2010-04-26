#' @include wrapped.learner.r
#' @include wrapped.learner.classif.r
#' @include wrapped.learner.regr.r
#' @include resample.instance.r
#' @include train.learner.r
#' @include predict.learner.r

setClass(
		"opt.wrapper",
		contains = c("wrapped.learner"),
		representation = representation(
				type = "character",
				base.learner = "wrapped.learner",
				method = "character",
				resampling = "resample.desc",
				control = "ANY",
				measures = "list",
				aggr = "list"
		)
)

setClass(
		"opt.wrapper.classif",
		contains = c("opt.wrapper", "wrapped.learner.classif")
)

setClass(
		"opt.wrapper.regr",
		contains = c("opt.wrapper", "wrapped.learner.regr")
)


#' Constructor.
#' @title Constructor for tuning wrapper
setMethod(
		f = "initialize",
		signature = signature("opt.wrapper"),
		def = function(.Object, type, base.learner, id, label, resampling, method, control, measures, aggr) {
			if (missing(base.learner))
				return(.Object)
			bl = base.learner
			.Object@base.learner = bl
			.Object@method = method
			.Object@resampling = resampling
			.Object@control = control
			.Object@measures = measures
			.Object@aggr = aggr
			callNextMethod(.Object, id=id, label=label, pack="mlr", props=bl@props)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="opt.wrapper", 
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
			if (type == "tune")
				or = tune(bl, task=lt, resampling=wl@resampling, method=wl@method, control=wl@control, 
						measures=wl@measures, aggr=wl@aggr, model=TRUE)
			else
				or = varsel(bl, task=lt, resampling=wl@resampling, method=wl@method, control=wl@control, 
						measures=wl@measures, aggr=wl@aggr, model=TRUE)
			
			m = tr@model["learner.model"]
			attr(m, "opt") = tr@opt
			attr(m, "path") = tr@path
			return(m)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "opt.wrapper", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			predict.learner(.wrapped.learner@base.learner, .wrapped.model, .newdata, .type, ...)
		}
)	

