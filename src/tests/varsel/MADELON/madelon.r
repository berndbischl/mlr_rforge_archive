setwd("d:/sync/projekte/mlr/")
#X = read.table("src/tests/varsel/madelon/madelon_train.data", colClasses="numeric")
#y = read.table("src/tests/varsel/madelon/madelon_train.labels")
#y = as.factor(y[,1,drop=T])
#mydata = cbind(X,y) 

source("src/files.r")
source("src/varsel/forward.r")
load.all.libs()
load.all.sources("src")
logger.define(level="error", global=T)
parallel.setup(global=TRUE)


ct <- make.classif.task("rpart.classif", data=mydata, formula=y~.)
#cv.i <- make.cv.instance(size=nrow(mydata), iters=3)
rd = make.subsample.desc(iters=10, split=0.5)
sel <- forward.sel(ct, resample.desc=rd)

