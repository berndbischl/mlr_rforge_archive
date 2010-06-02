#' @include base.wrapper.r
#' @include resample.desc.r

setClass(
		"opt.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
				opttype = "character",
				method = "character",
				resampling = "resample.desc",
				control = "ANY",
				measures = "list",
				aggr = "list"
		)
)


#' Constructor.
#' @title Constructor for tuning wrapper
setMethod(
		f = "initialize",
		signature = signature("opt.wrapper"),
		def = function(.Object, opttype, learner, resampling, method, control, measures, aggr) {
			if (missing(learner))
				return(.Object)
			.Object@opttype = opttype
			.Object@method = method
			.Object@resampling = resampling
			.Object@control = control
			.Object@measures = measures
			.Object@aggr = aggr
			callNextMethod(.Object, learner)
		}
)


#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="opt.wrapper", 
				.targetvar="character", 
				.data="data.frame", 
				.data.desc="data.desc", 
				.task.desc="task.desc", 
				.weights="numeric", 
				.costs="ANY" 
		),
		
		def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
			wl = .learner
			bl = wl@learner
			
			lt = make.task(data=.data, target=.targetvar)	
			if (wl@opttype == "tune")
				or = tune(bl, task=lt, resampling=wl@resampling, method=wl@method, control=wl@control, 
						measures=wl@measures, aggr=wl@aggr, model=TRUE)
			else if (wl@opttype == "varsel")
				or = varsel(bl, task=lt, resampling=wl@resampling, method=wl@method, control=wl@control, 
						measures=wl@measures, aggr=wl@aggr, model=TRUE)
			else 
				stop("Unknown type: ", wl@opttype)
			
			m = or@model["learner.model"]
			attr(m, "opt.result") = or
			return(m)
		}
)


make.opt.wrapper = function(opttype, learner, resampling, method, control, measures, aggr) {
	if (is.character(learner))
		learner = make.learner(learner)
	if (missing(measures))
		measures = default.measures(learner)
	measures = make.measures(measures)
	if (missing(aggr))
		aggr = default.aggr()
	aggr = make.aggrs(aggr)
	new("opt.wrapper", opttype, learner, resampling, method, control, measures, aggr=aggr)
}


