source("src/files.r")
load.all.libs()
load.all.sources("src")
parallel.setup()

#library(mlr)
logger.setup(level="warn")


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

set.seed(1)
library(mlbench)
data(BreastCancer)
mydata <- na.omit(BreastCancer[,-1])

ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Class~.)
subs.i <- make.subsample.instance(size=nrow(mydata), iters=3, split=2/3)
#control = pattern.control(maxit=20)
n <- 5
lower=c(0,0)
C <- sample(log10(1:1000), 1)
sigma <- sample(log10(1:1000), 1)
results <- list()

i=1
#for (i in 1:n) {
	start <- list(C=C[i], sigma=sigma[i])
	r <- tune.optim(ct, subs.i, method="cmaes", start=start, lower=lower, control=pattern.control(maxit=10))
	results <- c(results, list(r))
#}	

res.i = make.subsample.instance(size=nrow(mydata), iters=30, split=2/3)
rf = resample.fit(ct, res.i, parset=results[[1]]$par)
print(resample.performance(ct, res.i, rf))



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
		