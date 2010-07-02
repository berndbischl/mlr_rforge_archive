source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.define(level="warn", global=T)

parallel.setup(global=TRUE)

data(BostonHousing)


ct <- make.classif.task("loclda", data=iris, formula=Species~.)
cv.i <- make.cv.instance(size=nrow(iris), iters=2)
#rf <- resample.fit(ct, cv.i, type="prob")

my.f <- function(gamma) {function(x) exp(-x*gamma)}
ranges = list(weight.func=sapply(1:3, my.f))
tune(ct, cv.i, ranges)

