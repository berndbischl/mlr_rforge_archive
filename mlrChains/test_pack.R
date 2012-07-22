library(mlr)
library(mlbench)
data(BostonHousing)

configureMlr(on.learner.error="stop", show.learner.output=FALSE)


df1 = iris
df2 = BostonHousing
target1 = "Species"
target2 = "medv"
task1 = makeClassifTask(data=df1, target=target1)
task2 = makeRegrTask(data=df2, target=target2)

lrn1 = makeLearner("classif.fnn")
lrn2 = makeLearner("regr.ridge")

m2 = train(lrn2, task2)
p2 = predict(m2, task2)
print(performance(p2, mse))
d = as.data.frame(p2)

#rdesc = makeResampleDesc("CV", iters=2)
#rin = makeResampleInstance(rdesc, task=task1)
#r1 = resample(lrn1, task1, rdesc)
#print(r1$aggr)
#d = as.data.frame(r1$pred)

