#' @include base.wrapper.r
#' @include resample.desc.r

setClass(
		"opt.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
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
		def = function(.Object, learner, resampling, control, measures, aggr) {
			if (missing(learner))
				return(.Object)
			.Object@resampling = resampling
			.Object@control = control
			.Object@measures = measures
			.Object@aggr = aggr
			callNextMethod(.Object, learner)
		}
)


#' @rdname opt.result-class

setMethod(
		f = "[",
		signature = signature("opt.wrapper"),
		def = function(x,i,j,...,drop) {
			if (i == "opt.type"){
				return(x@control["opt.type"])
			}
			callNextMethod()
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
			if (wl["opt.type"] == "tune")
				or = tune(bl, task=lt, resampling=wl@resampling, control=wl@control, 
						measures=wl@measures, aggr=wl@aggr, model=TRUE)
			else if (wl["opt.type"] == "varsel")
				or = varsel(bl, task=lt, resampling=wl@resampling, control=wl@control, 
						measures=wl@measures, aggr=wl@aggr, model=TRUE)
			else 
				stop("Unknown type: ", wl["opt.type"])
			
			m = or@model["learner.model"]
			attr(m, "opt.result") = or
			return(m)
		}
)


make.opt.wrapper = function(learner, resampling, control, measures, aggr) {
	if (is.character(learner))
		learner = make.learner(learner)
	if (missing(measures))
		measures = default.measures(learner)
	measures = make.measures(measures)
	if (missing(aggr))
		aggr = default.aggr()
	aggr = make.aggrs(aggr)
	new("opt.wrapper", learner, resampling, control, measures, aggr=aggr)
}


