


test.nb <- function() {

  m <- naiveBayes(formula=multiclass.formula, data=multiclass.train)
  p  <- predict(m, newdata=multiclass.test[,-multiclass.class.col])
  p2 <- predict(m, newdata=multiclass.test[,-multiclass.class.col], type="raw")
  
  simple.test("classif.naiveBayes", multiclass.df, multiclass.target, multiclass.train.inds, p)
  prob.test  ("classif.naiveBayes", multiclass.df, multiclass.target, multiclass.train.inds, p2)
  
  tt <- "naiveBayes"
  tp <- function(model, newdata) predict(model, newdata[,-multiclass.class.col])

  cv.test("classif.naiveBayes", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp )

}