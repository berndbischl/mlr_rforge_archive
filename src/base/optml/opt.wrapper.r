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
        measures = "list",
        opt.pars = "ParameterSet",
        control = "opt.control",
        log.fun = "function"
    )
)


#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("opt.wrapper"),
		def = function(.Object, learner, resampling, measures, par.set, control, log.fun) {
			if (missing(learner))
				return(.Object)
			.Object@resampling = resampling
      .Object@measures = measures
      .Object@opt.pars = par.set
      .Object@control = control
      .Object@log.fun = log.fun
      .Object = callNextMethod(.Object, learner, par.set=makeParameterSet(), par.vals=list())
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
      .task="learn.task", .subset="integer"
    ),
      
		def = function(.learner, .task, .subset,  ...) {
			wl = .learner
			bl = wl@learner
			ctrl = wl@control
      
      lt = subset(.task, .subset)
			if (wl["opt.type"] == "tune") {
				or = tune(bl, lt, wl@resampling, wl@measures, wl@opt.pars, ctrl)
        bl = set.hyper.pars(bl, par.vals=or@x)
        m = train(bl, lt)
      } else if (wl["opt.type"] == "varsel") {
				or = varsel(bl, lt, wl@resampling, control=ctrl, measures=wl@measures)
        lt = subset(lt, vars=or@x)
        m = train(bl, lt)
      }	else 
				stop("Unknown type: ", wl["opt.type"])
				
			# set the opt result as attribute, so we can extract it later 
			attr(m, "opt.result") = or
			return(m)
		}
)


make.opt.wrapper = function(learner, resampling, measures, par.set, control, log.fun) {
	if (is.character(learner))
		learner = make.learner(learner)
	if (missing(measures))
		measures = default.measures(learner)
  if (is(measures, "measure"))
    measures = list(measures)   
	new("opt.wrapper", learner, resampling, measures, par.set, control, log.fun)
}


