checkWeightsAndBlocking = function(data, target, weights, blocking) {
  if(!is.null(weights) && length(weights) != nrow(data))
    stop("Weights have to be of the same length as number of rows in data! Or pass none at all.")
  if(!is.null(blocking) && length(blocking) != nrow(data))
    stop("Blocking has to be of the same length as number of rows in data! Or pass none at all.")
}


requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id))
}

measureAggrName = function(measure) {
  paste(measure$id, measure$aggr$id, sep=".")
}

#FIXME do we need this? BBmisc?
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
