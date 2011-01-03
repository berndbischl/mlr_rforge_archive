#' @include base.wrapper.r
roxygen()
#' @include resample.desc.r
roxygen()
#' @include opt.control.r
roxygen()


#' Abstract base class to wrap an optimization algorithm around a learner.

setClass(
		"opt.wrapper",
		contains = c("base.wrapper"),
		representation = representation(
				resampling = "resample.desc",
				control = "opt.control",
				measures = "list"
		)
)


#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("opt.wrapper"),
		def = function(.Object, learner, id, resampling, control, measures) {
			if (missing(learner))
				return(.Object)
			.Object@resampling = resampling
			.Object@control = control
			.Object@measures = measures
			callNextMethod(.Object, learner, id, par.descs=list(), par.vals=list())
		}
)


#' @rdname opt.wrapper-class

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
      .task="learn.task", .subset="integer", .vars="character"
    ),
      
		def = function(.learner, .task, .subset, .vars,  ...) {
			wl = .learner
			bl = wl@learner
			ctrl = wl@control
			
			lt = make.task(data=.data, target=.targetvar)	
			if (wl["opt.type"] == "tune")
				or = tune(bl, task=lt, resampling=wl@resampling, control=ctrl, 
						measures=wl@measures, model=TRUE)
			else if (wl["opt.type"] == "varsel")
				or = varsel(bl, task=lt, resampling=wl@resampling, control=ctrl, 
						measures=wl@measures, model=TRUE)
			else 
				stop("Unknown type: ", wl["opt.type"])
				
			m = or@model["learner.model"]
			# we dont need the model as we directly return it
			or@model = new("wrapped.model")
			# set the opt result as attribute, so we can extract it later 
			attr(m, "opt.result") = or
			return(m)
		}
)


make.opt.wrapper = function(learner, id, resampling, control, measures) {
	if (is.character(learner))
		learner = make.learner(learner)
	if (missing(measures))
		measures = default.measures(learner)
  if (is(measures, "measure"))
    measures = list(measures)   
	new("opt.wrapper", learner, id, resampling, control, measures)
}


