makeEnsemble <- function(...){
  learners = list(...)
  learner.names = sapply(learners, function(d) d$id)
  names(learners) = learner.names
  learners
  }
  
#l1 = makeLearner("regr.km")
#l2 = makeLearner("regr.rsm")
#l3 = makeLearner("regr.nnet")
#
#learner = makeRLEnsemble(l1,l2,l3)