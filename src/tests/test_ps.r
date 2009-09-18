source("src/files.r")
load.all.libs()
load.all.sources("src")
logger.define(level="error", global=T)
parallel.setup(global=TRUE)

mydata <- iris

ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Species~.)
cv.i <- make.subsample.instance(size=nrow(mydata), iters=30, split=2/3)
start <- list(C=1, sigma=1)
tune.ps(ct, cv.i, start=start)


#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Species~.)
#cv.i <- make.subsample.instance(size=nrow(mydata), iters=30, split=2/3)
#ranges <- list(C=seq(0.1, 2, by=0.1), sigma=seq(0.1, 2, 0.1))
#tune(ct, cv.i, ranges=ranges)





