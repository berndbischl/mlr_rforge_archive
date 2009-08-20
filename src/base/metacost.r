
library(RUnit)
library(MASS)
library(rpart)
library(e1071)
library(boot)
library(roxygen)
library(kernlab)
library(adabag)
library(kknn)
library(randomForest)
library(mda)
library(mlbench)
library(reshape)
library(klaR)

source("src/testsuite.config.r")

source("src/log/log.r")

source("src/base/classif.props.r")
source("src/base/data.desc.r")
source("src/base/learn.task.r")
source("src/base/model.r")
source("src/base/learner.failure.r")
source("src/base/performance.r")
source("src/base/check.learn.task.r")

source("src/base/resample.desc.r")
source("src/base/cv.desc.r")
source("src/base/bs.desc.r")
source("src/base/subsample.desc.r")

source("src/base/resample.run.r")
source("src/base/cv.run.r")
source("src/base/bs.run.r")
source("src/base/subsample.run.r")

source("src/base/resample.result.r")
source("src/base/resample.fit.r")
source("src/base/resample.performance.r")

source("src/base/benchmark.r")
source("src/base/tune.r")



source("src/models/lda.r")
source("src/models/qda.r")
source("src/models/knn.r")
source("src/models/rpart.r")
source("src/models/randomForest.r")
source("src/models/boost.r")
source("src/models/svm.r")
source("src/models/nb.r")
source("src/models/mda.r")
source("src/models/rda.r")

source("src/runit/runit.prefs.r")
source("src/runit/helpers.r")
source("src/runit/make.runit.tests.r")


ct = new("t.lda", data=iris, formula=Species~.)

parset = list()
iters = 1
data= ct@data

size = nrow(data)

bs.run = make.bs.run(size=nrow(data), iters=iters)

rf = resample.fit(ct, resample.run=bs.run)

class.nr = ct["class.nr"]
probs = array(dim=c(iters, size, class.nr))

for (i in 1:iters) {
	probs[i,,] = predict(rf@models[[i]], newdata=data, type="prob")
}


data2 = data

ct2 = ct
ct2@data = data2
m = train(ct2)

