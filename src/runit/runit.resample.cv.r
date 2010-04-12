

test.cv <- function() {

  data <- multiclass.df
  formula <- multiclass.formula
  parset <- list(minsplit=12, cp=0.09)

  tt <- "rpart"
  tp <- function(model, newdata) predict(model, newdata, type="class")

  cv.test("classif.rpart", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp, parset=parset)
}







