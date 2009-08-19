

test.bs <- function() {

  data <- multiclass.df
  formula <- multiclass.formula
  iters <- 4
  parset <- list(minsplit=12, cp=0.09)

  tt <- "rpart"
  tp <- function(model, newdata) predict(model, newdata, type="class")

  bs.test("rpart.classif", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp, parset=parset)
}







