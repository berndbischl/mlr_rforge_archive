

source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.define(level="error", global=T)

parallel.setup(global=TRUE)

data(iris)

# set up the learning task and parameter grid
 
fr <- mlbench.friedman1(150)
fr2 <- as.data.frame(fr$x)
fr2$y <- fr$y 
rd <- fr2  


xx = cvl (y ~., lambda2 = 5, data=rd, fold = 5)

rt <- make.regr.task("penalized.ridge", formula=y~., data=rd)
cv <- make.cv.instance(size=150, iters=5)
rf <- resample.fit(rt, cv)


#train.inds <- seq(1,150,2)
# test.inds <- seq(2,150,2)
#
# ct <- make.classif.task("lda", data=iris, formula=Species~.)
# cm <- train(ct, subset=train.inds)
# ps <- predict(ct, cm, newdata=iris[test.inds,])
# 
# ct <- make.classif.task("kknn.knn.classif", data=iris, formula=Species~.)
# cm <- train(ct, subset=train.inds, parset=list(k=3))
# ps <- predict(ct, cm, newdata=iris[test.inds,])
#
## result
#
#print(rr)

#print(resample.performance(rt,rin, rr, measure="mse"))
#tr <- tune(rt, rin, ranges=list(k=2:5), measure="mse")
#print(tr)

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







