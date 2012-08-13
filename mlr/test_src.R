library("devtools")
library("testthat")
library(mlbench)
library(ROCR)
data(BostonHousing)

load_all("skel")
source("skel/inst/tests/objects.R")

configureMlr(on.learner.error="stop", show.learner.output=FALSE)

#df = binaryclass.df
#df[, 1] = as.factor(sample(1:3, nrow(df), replace=TRUE))
#df[, 2] = as.factor(sample(1:3, nrow(df), replace=TRUE))
#df = df[, c(1,2,3, ncol(df))]
#task = makeClassifTask(data=df, target=binaryclass.target)


df1 = iris
df2 = BostonHousing
target1 = "Species"
target2 = "medv"
target3 = "Class"
task1 = makeClassifTask(data=df1, target=target1)
task2 = makeRegrTask(data=df2, target=target2)
task = binaryclass.task
task = task1

task = subsetTask(task, subset=1:100)
rdesc = makeResampleDesc("Holdout", split=0.3, stratify=TRUE)
rin = makeResampleInstance(rdesc, task=task)
print(rin$train.inds)
print(rin$test.inds)

#lrn = makeLearner("classif.rpart", minsplit=55, predict.type="prob")
#m = train(lrn, task)
#p = predict(m, task)
#pp = asROCRPrediction(p)
#print(lrn)


#lrn = makeLearner("regr.lm", predict.type="se")
#m = train(lrn, task2)
#p = predict(m, task2)

#lrn = makeLearner("classif.ksvm", sigma=2)
#m = train(lrn, task)
#p = predict(m, task)
#p = predict(m, newdata=df)
#rdesc = makeResampleDesc("CV", iters=5)
#rin = makeResampleInstance(rdesc, task=task)
#r = resample(lrn, task, rdesc)
#print(r$aggr)

#f = function(lrn) {
#  lrn2 = makeLearner(lrn)
#  m = train(lrn2, task)
#  p = predict(m, task)
#  p = predict(m, newdata=df)
#  #lrn2 = makeLearner(lrn, predict.type="prob")
#  #m = train(lrn2, task)
#  #p = predict(m, task)
#  #p = predict(m, newdata=df)
#}
  
  
#f("classif.ada")
#f("classif.blackboost")

#f("classif.lda")
