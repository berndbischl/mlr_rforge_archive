source("src/files.r")
load.all.libs()
load.all.sources("src")
logger.setup(level="error")
parallel.setup()


# Four datasets are tested: Iris, BreastCancer, Glass, Liver

library(cmaes)

# Dataset Iris
#-------------------------

#mydata <- iris
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Species~.)
#cv.i <- make.subsample.instance(size=nrow(mydata), iters=30, split=2/3)
#start <- list(C=1, sigma=1)
#res <- tune.optim(ct, cv.i, method="cmaes", start=start, 
#		lower=c(10^(-6), 10^(-6)), control=list(maxit=2)) 




# Dataset BreastCancer
#-------------------------

#library(mlbench)
#data(BreastCancer)
#mydata <- na.omit(BreastCancer[,-1])
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Class~.)
#subs.i <- make.subsample.instance(size=nrow(mydata), iters=20, split=2/3)
#start <- list(C=1, sigma=1)
#res <- tune.optim(ct, subs.i, method="cmaes", start=start, 
#		lower=c(10^(-6), 10^(-6)), control=list(maxit=2)) 





# Dataset Glass
#-------------------------

#library(mlbench)
#data(Glass)
#mydata <
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Type~.)
#cv.i <- make.cv.instance(size=nrow(mydata), iters=10)
#start <- list(C=1, sigma=1)
#res <- tune.optim(ct, cv.i, method="cmaes", start=start, 
#		lower=c(10^(-6), 10^(-6)), control=list(maxit=2)) 




# Dataset Liver
#-------------------------

#liver <- read.table("C:\\user\\borg\\workspace\\mlr\\src\\tests\\tune\\liver.data", sep=",")
#mydata <- liver
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=V7~.)
#r.i <- make.cv.instance(size=nrow(mydata), iters=10)
#start <- list(C=1, sigma=1)
#res <- tune.optim(ct, r.i, method="cmaes", start=start, 
#		lower=c(10^(-6), 10^(-6)), control=list(maxit=2)) 

#plot(erg$path$val)
#
#plot(erg$path$C, erg$path$sigma, type="b", xlim=c(0,2), ylim=c(0,2))