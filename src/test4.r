

source("src/files.r")
load.all.libs()
load.all.sources("src")

logger.setup(level="error")

parallel.setup(mode="local")

library(mlbench)
data(BostonHousing)

rt <- make.regr.task("penalized.lasso", data=BostonHousing, target="medv")
rd = make.cv.desc(iters=5)
rf = resample.fit(rt, rd)
#m=train(ct)
#predict(ct, m)