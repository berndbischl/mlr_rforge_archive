


test.qda <- function() {

  m <- try(qda(formula=testsuite.formula, data=testsuite.train))
  if(class(m)!="try-error") {
   p <- predict(m, newdata=testsuite.test) 
  } else {
   p <- m 
  }

  simple.test("qda", testsuite.df,testsuite.formula, testsuite.train.inds, p$class)
  prob.test  ("qda", testsuite.df,testsuite.formula, testsuite.train.inds, p$posterior)
  
  tt <- "qda"
  tp <- function(model, newdata) predict(model, newdata)$class

  cv.test("qda", testsuite.df, testsuite.formula, tune.train=tt, tune.predict=tp )
}
