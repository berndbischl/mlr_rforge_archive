checkBlocking = function(data, target, blocking) {
  if(length(blocking) > 0 && length(blocking) != nrow(data))
    stop("Blockings have to be of the same length as number of rows in data! Or pass none at all.")
}

requireLearnerPackages = function(learner) {
  requirePackages(learner$package, paste("learner", learner$id))
}
