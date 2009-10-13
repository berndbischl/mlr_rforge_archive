liver <- read.table("C:\\user\\borg\\workspace\\mlr\\src\\tests\\tune\\liver.data", sep=",")
liver_data <- data[,-7]
liver_label <- data[,7] 



mydata <- liver

ct <- make.classif.task("kernlab.svm.classif", data=mydata, formula=V7~.)
bs.i <- make.bs.instance(size=nrow(mydata), iters=5)
start <- list(C=1, sigma=1)
tune.ps(ct, bs.i, start=start)






