


test.lda <- function() {

  m <- lda(formula=testsuite.formula, data=testsuite.train)
  p <- predict(m, newdata=testsuite.test)

  simple.test("lda", testsuite.df, testsuite.formula, testsuite.train.inds, p$class)
#  prob.test  ("lda", testsuite.df, testsuite.formula, testsuite.train.inds, p$posterior)
#  
#  tt <- "lda"
#  tp <- function(model, newdata) predict(model, newdata)$class
#  
#  cv.test("lda", testsuite.df, testsuite.formula, tune.train=tt, tune.predict=tp )

}
