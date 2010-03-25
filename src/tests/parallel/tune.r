

library(mlr)
library(mlbench)
data(BreastCancer)

ct = make.classif.task(data=na.omit(BreastCancer), target="Class", excluded="Id")
res = make.res.desc("subsample", iters=50)
ranges = list(C=-1:1, sigma=-1:1)
ctrl = grid.control(ranges=ranges)


parallel.setup(mode="local")
st1 = system.time({
	tr1 = tune("kernlab.svm.classif", ct, res, method="grid", control=ctrl, scale=function(x)10^x)
})

parallel.setup(mode="sfCluster", level="tune")
st2 = system.time({
	tr2 = tune("kernlab.svm.classif", ct, res, method="grid", control=ctrl, scale=function(x)10^x)
})


parallel.setup(mode="sfCluster", level="resample")
st3 = system.time({
	tr3 = tune("kernlab.svm.classif", ct, res, method="grid", control=ctrl, scale=function(x)10^x)
})

print(c(st1[3], st2[3], st3[3]))
print(c(tr1$perf, tr2$perf, tr3$perf))
