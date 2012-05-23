library("devtools")
library("testthat")
library(mlbench)

load_all("skel")
source("skel/inst/tests/objects.R")

df = binaryclass.df
df[, 1] = as.factor(sample(1:3, nrow(df), replace=TRUE))
df[, 2] = as.factor(sample(1:3, nrow(df), replace=TRUE))
df = df[, c(1,2,3, ncol(df))]
task = makeClassifTask(data=df, target=binaryclass.target)

lrn = makeLearner("classif.lda")
m = train(lrn, task)
p = predict(m, task)
p = predict(m, newdata=df)


f = function(lrn) {
  lrn2 = makeLearner(lrn)
  m = train(lrn2, task)
  p = predict(m, task)
  p = predict(m, newdata=df)
  #lrn2 = makeLearner(lrn, predict.type="prob")
  #m = train(lrn2, task)
  #p = predict(m, task)
  #p = predict(m, newdata=df)
}
  
  
#f("classif.ada")
#f("classif.blackboost")

#f("classif.lda")
