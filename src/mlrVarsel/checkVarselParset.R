checkVarselParset = function(learner, par.set, bit.names, control) {
  # cannot do this in varselwrapper
  #if (is(learner, "Varsel")length(bit.names) == 0)
  #  stop("No features to select!")
  if (is(control, "TuneControlGrid")) {
  } else if (is(control, "VarselControlSequential")) {
  } else if (is(control, "VarselControlRandom")) {
  } else if (is(control, "VarselControlExhaustive")) {
  } else {
    stop(paste("Varsel algorithm for", class(control)[1], "does not exist!"))
  }
}
