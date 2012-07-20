checkWeightsAndBlocking = function(data, target, weights, blocking) {
  if(length(weights) > 0 && length(weights) != nrow(data))
    stop("Weights have to be of the same length as number of rows in data! Or pass none at all.")
  if(length(blocking) > 0 && length(blocking) != nrow(data))
    stop("Blocking has to be of the same length as number of rows in data! Or pass none at all.")
}


requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id))
}

measureAggrName = function(measure) {
  paste(measure$id, measure$aggr$id, sep=".")
}

perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits=3), sep=""), collapse=",")
}

recodeY = function(y, type, positive) {
  if (type == "01")
    as.numeric(y == positive)
  else if (type == "-1+1")
    2*as.numeric(y == positive)-1
  else
    y
}
