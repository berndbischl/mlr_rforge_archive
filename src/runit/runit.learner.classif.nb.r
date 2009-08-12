


test.nb <- function() {

  m <- naiveBayes(formula=testsuite.formula, data=testsuite.train)
  p  <- predict(m, newdata=testsuite.test[,-testsuite.class.col])
  p2 <- predict(m, newdata=testsuite.test[,-testsuite.class.col], type="raw")
  
  simple.test("classif.nb", testsuite.df, testsuite.formula, testsuite.train.inds, p)
  prob.test  ("classif.nb", testsuite.df, testsuite.formula, testsuite.train.inds, p2)
  
  tt <- "naiveBayes"
  tp <- function(model, newdata) predict(model, newdata[,-testsuite.class.col])

  cv.test("classif.nb", testsuite.df, testsuite.model, tune.train=tt, tune.predict=tp )

}
