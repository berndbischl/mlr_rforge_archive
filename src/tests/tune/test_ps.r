source("src/files.r")
load.all.libs()
load.all.sources("src")
logger.setup(level="error")
parallel.setup()


# Four datasets are tested: Iris, BreastCancer, Glass, Liver


# Dataset Iris
#-------------------------

#mydata <- iris
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Species~.)
#cv.i <- make.subsample.instance(size=nrow(mydata), iters=5, split=2/3)
#control = pattern.control(maxit=20)
#start <- list(C=1, sigma=1)
#lower=c(0,0)
#erg <- tune.optim(ct, cv.i, method="pattern", start=start, lower=lower, control=control)





# Dataset BreastCancer
#-------------------------

#library(mlbench)
#data(BreastCancer)
#mydata <- na.omit(BreastCancer[,-1])
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Class~.)
#subs.i <- make.subsample.instance(size=nrow(mydata), iters=20, split=2/3)
#control = pattern.control(maxit=20)
#start <- list(C=1, sigma=1)
#lower=c(0,0)
#erg <- tune.optim(ct, subs.i, method="pattern", start=start, lower=lower, control=control)
			





# Dataset Glass
#-------------------------

#library(mlbench)
#data(Glass)
#mydata <- Glass
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Type~.)
#cv.i <- make.cv.instance(size=nrow(mydata), iters=10)
#control = pattern.control(maxit=20)
#start <- list(C=1, sigma=1)
#lower=c(0,0)
#erg <- tune.optim(ct, cv.i, method="pattern", start=start, lower=lower, control=control)





# Dataset Liver
#-------------------------

#liver <- read.table("C:\\user\\borg\\workspace\\mlr\\src\\tests\\tune\\liver.data", sep=",")
#mydata <- liver
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=V7~.)
#r.i <- make.cv.instance(size=nrow(mydata), iters=10)
#control = pattern.control(maxit=20)
#start <- list(C=1, sigma=1)
#lower=c(0,0)
#erg <- tune.optim(ct, cv.i, method="pattern", start=start, lower=lower, control=control)

#plot(erg$path$val)
#
#plot(erg$path$C, erg$path$sigma, type="b", xlim=c(0,2), ylim=c(0,2))
		