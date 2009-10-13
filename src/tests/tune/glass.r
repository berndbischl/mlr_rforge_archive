# Data

library(mlbench)
data(Glass)
glass <- Glass[,-10]
glass_label <- Glass[,10]


# Tuning

mydata <- Glass

ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Type~.)
cv.i <- make.cv.instance(size=nrow(mydata), iters=5)
start <- list(C=1, sigma=1)
tune.ps(ct, cv.i, start=start)