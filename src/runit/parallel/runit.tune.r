
library(mlr)
library(mlbench)

ct = make.task(data=iris, target="Species")
res = make.res.desc("cv", iters=2)


ctrl = grid.control(ranges=list(sigma=c(0.1, 1, 10)))

parallel.setup(mode="local")
tr1 = tune("classif.ksvm", task=ct, resampling=res, control=ctrl, path=T)
parallel.setup(mode="multicore", cpus=2, level="resample")
tr2 = tune("classif.ksvm", task=ct, resampling=res, control=ctrl, path=T)
parallel.setup(mode="multicore", cpus=2, level="tune")
tr3 = tune("classif.ksvm", task=ct, resampling=res, control=ctrl, path=T)


for (i in 1:3) {
  checkEquals(tr1["path", as.data.frame=T], tr2["path", as.data.frame=T])
  checkEquals(tr2["path", as.data.frame=T], tr3["path", as.data.frame=T])
}
