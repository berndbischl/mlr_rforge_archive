
test.svm.classif <- function() {
  library(e1071)
  set.seed(debug.seed)
  m1 = svm(multiclass.formula, data=multiclass.train, kernel="radial", gamma=20)
  set.seed(debug.seed)
  m2 = svm(multiclass.formula, data=multiclass.train, kernel="radial", gamma=20, probability = TRUE)
  p1 = predict(m1, newdata=multiclass.test)
  p2 = predict(m2, newdata=multiclass.test, probability=TRUE)
  simple.test("classif.svm", multiclass.df, multiclass.target, multiclass.train.inds, p1,  parset=list(kernel="radial", gamma=20))
  prob.test  ("classif.svm", multiclass.df, multiclass.target, multiclass.train.inds, attr(p2, "probabilities"), 
    parset=list(kernel="radial", gamma=20))
  
  set.seed(debug.seed)
  m = svm(multiclass.formula, data=multiclass.train, kernel="sigmoid", gamma=10, probability = TRUE)
  p = predict(m, newdata=multiclass.test, probability = TRUE)
  prob.test  ("classif.svm",multiclass.df, multiclass.target, multiclass.train.inds, attr(p2, "probabilities"), 
    parset=list(kernel="sigmoid", gamma=10))
  
  set.seed(debug.seed)
  m <- svm(multiclass.formula, data=multiclass.train, kernel="polynomial", kpar=list(degree=3, coef0=2, scale=1.5), probability = TRUE)
  p <- predict(m, newdata=multiclass.test)
  p2 <- predict(m, newdata=multiclass.test, type="prob")
  simple.test("classif.svm", multiclass.df, multiclass.target, multiclass.train.inds, p,  
    parset=list(kernel="polynomial", degree=3, coef0=2, gamma=1.5))
  prob.test  ("classif.svm", multiclass.df, multiclass.target, multiclass.train.inds, attr(p2, "probabilities"), 
    parset=list(kernel="polynomial", degree=3, coef0=2, gamma=1.5))
  
  tt <- function (formula, data, subset=1:150, ...) {
    svm(x=formula, data=data[subset,], kernel="polynomial", kpar=list(degree=3, coef0=2, scale=1.5))
  }
  
  cv.test("classif.svm", multiclass.df, multiclass.target, tune.train=tt, parset=list(kernel="polynomial", degree=3, coef0=2, scale=1.5))
  
}
