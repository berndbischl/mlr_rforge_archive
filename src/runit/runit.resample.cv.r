

test.cv <- function() {

  data <- testsuite.df
  formula <- testsuite.formula
  parset <- list(minsplit=12, cp=0.09)

  tt <- "rpart"
  tp <- function(model, newdata) predict(model, newdata, type="class")

  cv.test("rpart.classif", testsuite.df, testsuite.formula, tune.train=tt, tune.predict=tp, parset=parset)
}







