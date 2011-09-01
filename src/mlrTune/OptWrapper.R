#' Abstract base class to wrap an optimization algorithm around a learner.
#' @importClassesFrom mlr BaseWrapper Learner object

setClass(
		"OptWrapper",
		contains = c("BaseWrapper"),
		representation = representation(
				resampling = "ResampleDesc",
        measures = "list",
        opt.pars = "ParameterSet",
        bit.names = "character",
        bits.to.features = "function",
        control = "OptControl",
        log.fun = "function"
    )
)


#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("OptWrapper"),
		def = function(.Object, learner, resampling, measures, par.set, bit.names, bits.to.features, control, log.fun) {
			if (missing(learner))
				return(.Object)
			.Object@resampling = resampling
      .Object@measures = measures
      .Object@opt.pars = par.set
      .Object@bit.names = bit.names
      .Object@bits.to.features = bits.to.features
      .Object@opt.pars = par.set
      .Object@control = control
      .Object@log.fun = log.fun
      .Object = callNextMethod(.Object, learner, par.set=makeParameterSet(), par.vals=list())
      # set predict type of base learner
      setPredictType(.Object, learner@predict.type)
    }
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
    signature = signature(
      .learner="OptWrapper", 
      .task="LearnTask", .subset="integer"
    ),
      
		def = function(.learner, .task, .subset,  ...) {
			wl = .learner
			bl = wl@learner
			ctrl = wl@control
      # todo: strange error if we remove :::? maybe rename subset...
      lt = mlr:::subset(.task, .subset)
			if (is(wl@control, "TuneControl")) {
				or = tune(bl, lt, wl@resampling, wl@measures, wl@opt.pars, ctrl)
        bl = setHyperPars(bl, par.vals=or@x)
        m = train(bl, lt)
      } else if (is(wl@control, "VarselControl")) {
				or = varsel(bl, lt, wl@resampling, measures=wl@measures, wl@bit.names, wl@bits.to.features, control=ctrl)
        lt = subset(lt, vars=or@x)
        m = train(bl, lt)
      }	else 
				stop("Should not happen!")
			# set the opt result as attribute, so we can extract it later 
			attr(m, "opt.result") = or
			return(m)
		}
)
