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
library(snowfall)

source("src/testsuite.config.r")

source("src/log/log.r")

source("src/base/learner.props.r")
source("src/base/learner.props.classif.r")
source("src/base/learner.props.regr.r")
source("src/base/data.desc.r")

source("src/base/wrapped.learner.r")
source("src/base/wrapped.learner.classif.r")
source("src/base/wrapped.learner.regr.r")

source("src/base/task.learn.r")
source("src/base/task.classif.r")
source("src/base/task.classif.make.r")
source("src/base/task.regr.r")
source("src/base/task.regr.make.r")

source("src/base/check.task.classif.r")

source("src/base/wrapped.model.r")
source("src/base/learner.failure.r")

source("src/base/train.learner.r")
source("src/base/train.task.r")
source("src/base/train.task.classif.r")
source("src/base/train.task.regr.r")

source("src/base/predict.classif.r")
source("src/base/predict.regr.r")
source("src/base/conf.matrix.r")

source("src/base/prediction.r")
source("src/base/perf.measure.r")

source("src/base/resample.desc.r")
source("src/base/cv.desc.r")
source("src/base/bs.desc.r")
source("src/base/subsample.desc.r")
source("src/base/resample.instance.r")
source("src/base/cv.instance.r")
source("src/base/bs.instance.r")
source("src/base/subsample.instance.r")

source("src/base/resample.result.r")
source("src/base/resample.fit.r")
source("src/base/resample.fit.iter.r")
source("src/base/resample.performance.r")

source("src/base/tune.r")

source("src/base/parallel.r")

source("src/models/classification/lda.r")
source("src/models/classification/rpart.r")
source("src/models/classification/svm.r")
source("src/models/regression/gbm.r")
source("src/models/regression/kknn.r")

logger.define(level="error", global=T)

parallel.setup(global=TRUE)

data(BostonHousing)
rt <- make.regr.task("kknn.regr", data=BostonHousing, formula=medv~.)
rin <- make.cv.instance(iters=10, size=nrow(BostonHousing))
rr <- resample.fit(rt, rin)
print(rr)

print(resample.performance(rt,rin, rr, measure="mse"))
tr <- tune(rt, rin, ranges=list(k=2:5), measure="mse")
print(tr)

#print(tr)

#data(BreastCancer)
#
#df <- BreastCancer
#df <- df[,-1]
#df <- na.omit(df)
#ct <- make.classif.task("kernlab.svm.classif", data=df, Class~.)
#ri = make.cv.instance(size=nrow(df), iters=10)
#ranges = list(kernel="rbfdot", C=2^seq(-3,3), sigma=2^seq(-3,3))
#tr <- tune(ct, ri, ranges)
#
#
#
#source("src/models/classification/lda.r")
#source("src/models/classification/qda.r")
#source("src/models/classification/knn.r")
#source("src/models/classification/rpart.r")
#source("src/models/classification/randomForest.r")
#source("src/models/classification/boost.r")
#source("src/models/classification/svm.r")
#source("src/models/classification/nb.r")
#source("src/models/classification/mda.r")
#source("src/models/classification/rda.r")
#
#source("src/models/regression/blackboost.r")
#source("src/models/regression/gbm.r")
#
#
#source("src/runit/runit.prefs.r")
#source("src/runit/helpers.r")
#source("src/runit/make.runit.tests.r")
#
#source("src/base/parallel.r")
#source("src/base/perf.measure.r")
#source("src/base/var.sel.r")
#
#logger.define(level="error")
#parallel.setup()
#	
#load(file="c:/matthias/dset1.Rdata")
#
#
#dset2 = matrix(nrow=4, ncol=3)
#
#
#ct = new("class.svm", data=dset1, formula=instru~.)
#
#rin <- make.cv.instance(iters=5, size=nrow(dset1))
#
#tr <- tune(ct, resample.instance=rin, ranges=list(kernel=c("rbfdot", "vanilladot"), C=c(0.5,1,2)))
#
#







