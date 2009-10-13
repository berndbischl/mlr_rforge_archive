# Prepare data

library(mlbench)
data(BreastCancer)
breast <- na.omit(BreastCancer[,-c(1,11)])
breast_label <- na.omit(BreastCancer[,11])


# Tuning

mydata <- na.omit(BreastCancer[,-1])

ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Class~.)
subs.i <- make.subsample.instance(size=nrow(mydata), iters=30, split=2/3)
start <- list(C=1, sigma=1)
tune.ps(ct, subs.i, start=start)


