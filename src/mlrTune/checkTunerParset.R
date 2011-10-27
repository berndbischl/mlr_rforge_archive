checkTunerParset = function(learner, par.set, control) {
  if (length(par.set@pars) == 0)
    stop("No parameters were passed!")
  x = setdiff(names(par.set@pars), names(getParameterSet(learner)@pars))
  if (length(x) > 0)
    stop("Can only tune parameters for which learner parameters exist: ", paste(x, collapse=","))

  if (is(control, "TuneControlGrid")) {
    if (!all(sapply(par.set@pars, function(x) x@type %in% c("discrete", "logical"))))
      stop("Grid search can only be applied to discrete and logical parameters!")
  } else if (is(control, "TuneControlOptim")) {
    if (any(sapply(par.set@pars, function(x) !(x@type %in% c("numeric", "integer", "numericvector", "integervector")))))
      stop("Optim can only be applied to numeric, integer, numericvector, integervector parameters!")
    low = lower(par.set)
    if (length(control@start) != length(low))
      stop(" Length of 'start' has to match number of parameters in 'par.set'!")
  } else if (is(control, "TuneControlCMAES")) {
    if (any(sapply(par.set@pars, function(x) !(x@type %in% c("numeric", "integer", "numericvector", "integervector")))))
      stop("CMAES can only be applied to numeric, integer, numericvector, integervector parameters!")
    low = lower(par.set)
    if (length(control@start) != length(low))
      stop(" Length of 'start' has to match number of parameters in 'par.set'!")
  } else if (is(control, "TuneControlSPO")) {
  } else {
    stop("Tuning algorithm for ", class(control)[1], " does not exist!")
  }
}
