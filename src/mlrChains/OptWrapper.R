#' Abstract base class to wrap an optimization algorithm around a learner.

setClass(
		"OptWrapper",
		contains = c("BaseWrapper"),
		representation = representation(
				resampling = "ANY",
        measures = "list",
        opt.pars = "ANY",
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
      .Object = callNextMethod(.Object, learner, par.set=makeParamSet(), par.vals=list())
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
      # todo: strange error if we remove :::? maybe rename subset...
      task = subsetData(.task, .subset)
			if (is(.learner@control, "TuneControl")) {
				or = tune(.learner@learner, task, .learner@resampling, .learner@measures, 
          .learner@opt.pars, .learner@control)
        # set optimal hyper pars in base learner
        .learner@learner = setHyperPars(.learner@learner, par.vals=or@x)
        m = train(.learner@learner, task)
      } else if (is(.learner@control, "VarselControl")) {
        if (length(.learner@bit.names) == 0) 
          or = varsel(.learner@learner, task, .learner@resampling, .learner@control,
           .learner@measures)
        else  
          or = varsel(.learner@learner, task, .learner@resampling, .learner@control,
            .learner@measures, .learner@bit.names, .learner@bits.to.features)
        task = subsetData(task, vars=or@x)
        m = train(.learner@learner, task)
      }	else 
				stop("Should not happen!")
			# set the opt result as attribute, so we can extract it later 
			attr(m, "opt.result") = or
			return(m)
		}
)
