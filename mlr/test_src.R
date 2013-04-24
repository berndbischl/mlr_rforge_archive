library("devtools")
library("testthat")
library(mlbench)
library(ROCR)
data(BostonHousing)

load_all("skel")
#source("skel/inst/tests/objects.R")

configureMlr(on.learner.error="stop", show.learner.output=FALSE)

d = iris
d[,1] = as.factor(d[,5])
task = makeClassifTask(data=d, target="Species")
lrn = makeLearner("classif.randomForest")
m = train(lrn, task)
nd = d
nd[,1] = as.character(nd[,1])
p = predict(m, newdata=nd)
print(p)

#df = binaryclass.df
#df[, 1] = as.factor(sample(1:3, nrow(df), replace=TRUE))
#df[, 2] = as.factor(sample(1:3, nrow(df), replace=TRUE))
#df = df[, c(1,2,3, ncol(df))]
#task = makeClassifTask(data=df, target=binaryclass.target)



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
