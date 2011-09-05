source("src/files.r")
load.all.libs()
load.all.sources("src")
parallel.setup()

#library(mlr)
setupLogger(level="warn")


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

#set.seed(1)
#library(mlbench)
#data(BreastCancer)
#mydata <- na.omit(BreastCancer[,-1])
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Class~.)
#subs.i <- make.subsample.desc(size=nrow(mydata), iters=1, split=2/3)
#control = pattern.control(delta=20)
#
#n <- 50
#
#
#lower=c(0,0)
#C <- sample(log10(1:1000), n)
#sigma <- sample(log10(1:1000), n)
#results <- list()
#
#
#for (i in 1:n) {
#	start <- list(C=C[i], sigma=sigma[i])
#	r <- tune.optim(ct, subs.i, method="pattern", start=start, lower=lower, control = control)
#	results <- c(results, list(r))
#}	

#res.i = make.subsample.instance(size=nrow(mydata), iters=30, split=2/3)
#rf = resample.fit(ct, res.i, parset=results[[1]]$par)
#print(resample.performance(ct, res.i, rf))



set.seed(1)
library(mlbench)
data(BreastCancer)
mydata <- na.omit(BreastCancer[,-1])

ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Class~.)
cv.i <- make.subsample.desc(iters=1, split=2/3)
control = pattern.control(delta=10)

n <- 42


lower=c(0,0)
C.seq = seq(-3,3, by=0.01)
sigma.seq = seq(-3,3, by=0.01)
C <- 10^sample(C.seq, n)
sigma <- 10^sample(sigma.seq, n)
results <- list()


for (i in 1:n) {
	print("#i")
	print(i)
	start <- list(C=C[i], sigma=sigma[i])
	r <- tune.optim(ct, cv.i, method="pattern", start=start, lower=lower, control = control)
	results <- c(results, list(r))
}











# Dataset Glass
#-------------------------

#set.seed(227)
#library(mlbench)
#data(Glass)
#mydata <- Glass
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=Type~.)
#subs.i <- make.subsample.instance(size=nrow(mydata), iters=3, split=2/3)
#control = pattern.control(delta=10)
#
#n <- 2
#
#lower=c(0,0)
#C <- sample(log10(1:1000), n)
#sigma <- sample(log10(1:1000), n)
#results <- list()
#
#for (i in 1:n) {
#	start <- list(C=C[i], sigma=sigma[i])
#	r <- tune.optim(ct, subs.i, method="pattern", start=start, lower=lower, control = control)
#	results <- c(results, list(r))
#}	



# Dataset Liver
#-------------------------

#liver <- read.table("C:\\user\\borg\\workspace\\mlr\\src\\tests\\tune\\liver.data", sep=",")
#mydata <- liver
#
#
#ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=V7~.)
#subs.i <- make.subsample.instance(size=nrow(mydata), iters=30, split=2/3)
#control = pattern.control(delta=10)
#
#n <- 50
#
#lower=c(0,0)
#C <- sample(log10(1:1000), n)
#sigma <- sample(log10(1:1000), n)
#results <- list()
#
#for (i in 1:n) {
#	start <- list(C=C[i], sigma=sigma[i])
#	r <- tune.optim(ct, subs.i, method="pattern", start=start, lower=lower, control = control)
#	results <- c(results, list(r))
#}



#plot(erg$path$val)
#
#plot(erg$path$C, erg$path$sigma, type="b", xlim=c(0,2), ylim=c(0,2))
		