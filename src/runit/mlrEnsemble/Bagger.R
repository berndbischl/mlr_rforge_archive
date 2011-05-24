testBagger = function() {
  bl = makeLearner("classif.rpart")
  w = makeBagger(learner=bl)   
  w = setHyperPars(w, minsplit=4)
  checkEquals(getHyperPars(w), list(minsplit=4))
  m = train(w, task=multiclass.task)
  mm = m@learner.model
  checkTrue(is.list(mm) && length(mm)==10)
  checkTrue(all(sapply(mm, function(x) is(x@learner.model, "rpart"))))
  p = predict(m, task=multiclass.task)
  checkTrue(performance(p, mmce) < 0.1)
} 

