checkBlocking = function(data, target, blocking) {
  if(length(blocking) && length(blocking) != nrow(data))
    stop("Blocking has to be of the same length as number of rows in data! Or pass none at all.")
}


requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id))
}

measureAggrName = function(measure) {
  paste(measure$id, measure$aggr$id, sep=".")
}

perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits=3L), sep=""), collapse=",")
}

recodeY = function(y, type, positive) {
  if (type == "01")
    as.numeric(y == positive)
  else if (type == "-1+1")
    as.numeric(2L*(y == positive)-1L)
  else
    y
}
