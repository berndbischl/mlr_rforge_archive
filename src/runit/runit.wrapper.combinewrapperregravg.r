test.makeCombineWrapperRegrAvg = function() {
  
  learners = list("regr.lm", "regr.rpart", "regr.randomForest")
  w = makeCombineWrapperRegrAvg(learners)
  
  m1 = train(learners[[1]], regr.task)
  m2 = train(learners[[2]], regr.task)
  m3 = train(learners[[3]], regr.task)
  m =  train(w, regr.task)
  p1 = predict(m1, regr.task)@df$response
  p2 = predict(m2, regr.task)@df$response
  p3 = predict(m3, regr.task)@df$response
  p = predict(m, regr.task)@df$response
  
  checkEquals((p1+p2+p3)/3, p)
}
