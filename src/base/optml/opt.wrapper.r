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
			.Object@type = type
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
				.costs="matrix" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs,  ...) {
			wl = .wrapped.learner
			bl = wl@base.learner
			if (is(bl, "wrapped.learner.classif"))
				f = make.task
			else if (is(bl, "wrapped.learner.regr"))
				f = make.task
			
			lt = f(data=.data, target=.targetvar)	
			if (wl@type == "tune")
				or = tune(bl, task=lt, resampling=wl@resampling, method=wl@method, control=wl@control, 
						measures=wl@measures, aggr=wl@aggr, model=TRUE)
			else if (wl@type == "varsel")
				or = varsel(bl, task=lt, resampling=wl@resampling, method=wl@method, control=wl@control, 
						measures=wl@measures, aggr=wl@aggr, model=TRUE)
			else 
				stop("Unknown type: ", wl@type)
			
			m = or@model["learner.model"]
			attr(m, "opt") = or@opt
			attr(m, "path") = or@path
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


make.opt.wrapper <- function(type, learner, id, label, resampling, method="sfs", control, measures, aggr) {
	if (is.character(learner))
		learner = make.learner(learner)
	if (missing(id))
		id = learner["id"]
	if (missing(label))
		label = id
	if (missing(measures))
		measures = default.measures(learner)
	measures = make.measures(measures)
	if (missing(aggr))
		aggr = default.aggr()
	aggr = make.aggrs(aggr)
	if (is(learner, "wrapped.learner.classif"))
		tt = new("opt.wrapper.classif", type=type, id=id, label=label, base.learner=learner, resampling=resampling, 
				method=method, control=control, measures=measures, aggr=aggr)
	else		
		tt = new("opt.wrapper.regr", type=type, id=id, label=label, base.learner=learner, resampling=resampling, 
				method=method, control=control, measures=measures, aggr=aggr)
	return(tt)
}


