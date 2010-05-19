

library(mlr)
library(mlbench)
data(BreastCancer)

# vorsicht, beim testen: wir können nur 9 cpus beim tuning aber mehr beim resampling nutzen!!!!

ct = make.task(data=na.omit(BreastCancer), target="Class", excluded="Id")
res = make.res.desc("subsample", iters=5)
ranges = list(C=2^(-1:1), sigma=2^(-1:1))
ctrl = grid.control(ranges=ranges)


parallel.setup(mode="local")
st1 = system.time({
	tr1 = tune("classif.ksvm", ct, res, method="grid", control=ctrl)
})

parallel.setup(mode="sfCluster", level="tune")
st2 = system.time({
	tr2 = tune("classif.ksvm", ct, res, method="grid", control=ctrl)
})


parallel.setup(mode="sfCluster", level="resample")
st3 = system.time({
	tr3 = tune("classif.ksvm", ct, res, method="grid", control=ctrl)
})

print(c(st1[3], st2[3], st3[3]))
print(c(tr1["perf"], tr2["perf"], tr3["perf"]))

# elapsed elapsed elapsed
# 108.643  18.096  22.776
# [1] 0.02938596 0.02947368 0.02710526


