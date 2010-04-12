


test.qda <- function() {

  m <- try(qda(formula=multiclass.formula, data=multiclass.train))
  if(class(m)!="try-error") {
   p <- predict(m, newdata=multiclass.test) 
  } else {
   p <- m 
  }

  simple.test("classif.qda", multiclass.df,multiclass.formula, multiclass.train.inds, p$class)
  prob.test  ("classif.qda", multiclass.df,multiclass.formula, multiclass.train.inds, p$posterior)
  
  tt <- "qda"
  tp <- function(model, newdata) predict(model, newdata)$class

  cv.test("classif.qda", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp )
}
