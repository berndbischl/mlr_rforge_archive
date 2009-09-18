source("src/files.r")
load.all.libs()
load.all.sources("src")
logger.define(level="debug", global=T)
parallel.setup(global=TRUE)


# make binary problem from iris
mydata <- iris[1:100,]
mydata$Species <- mydata$Species[,drop=T]

ct <- make.classif.task("ada", data=mydata, formula=Species~.)
cv.i <- make.cv.instance(size=nrow(mydata), iters=3)
ranges <- list(minsplit=5:8)
tune(ct, cv.i, ranges)




