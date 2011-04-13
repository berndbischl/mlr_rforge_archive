test.rocr = function() {
  w = makeLearner("classif.lda", predict.type="prob")
  m = train(w, binaryclass.task)
  p = predict(m, binaryclass.task)
  p = as.ROCR.prediction(p)
  #ROCR.plot(p)
}