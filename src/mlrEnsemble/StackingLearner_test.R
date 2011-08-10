library(roxygen)
library(mlr)
source("D:\\sync\\projekte\\mlr\\src\\mlrEnsemble\\BaseCombiner.R")

library(mlbench)
data(BostonHousing)
mytask = makeRegrTask(data=BostonHousing, target="medv")

w1 = makeLearner("regr.lm")
w2 = makeLearner("regr.rpart")
w3 = makeLearner("regr.earth")
learners = list(w1, w2, w3)
res.st = makeResampleDesc("CV", iters=10)
w.super = makeLearner("regr.lm")
w.st = makeStackingLearner(id="bla", learners, makeLearner("regr.lm"), w.super)

#m1 = train(w1, mytask)
#p1 = predict(m1, mytask)
#m2 = train(w2, mytask)
#p2 = predict(m2, mytask)
#m3 = train(w3, mytask)
#p3 = predict(m3, mytask)
#print(performance(p1, mse))
#print(performance(p2, mse))
#print(performance(p3, mse))
#print(m3@learner.model$super.model@learner.model)

myrin = makeResampleInstance(makeResampleDesc("Subsample", iters=10, split=4/5), task=mytask)
lapply(learners, function(x) {
    r = resample(x, mytask, myrin, measures=mse)
    message(x@id, " : ", r$aggr[1])
  })
r.st = resample(w.st, mytask, myrin, measures=mse)
print(r.st$aggr[1])

m.st = train(w.st, mytask)
print(m.st@learner.model$super.model@learner.model$coef)

