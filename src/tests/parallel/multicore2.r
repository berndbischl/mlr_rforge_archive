
library(mlr)
library(mlbench)

ct = make.classif.task(data=iris, target="Species")
wl = make.learner("randomForest.classif", ntree=30000)
res = make.res.desc("cv", iters=10)


r = list(ntree=30000:30007)
ctrl = grid.control(ranges=r)

parallel.setup(mode="local")
st1 = system.time({		
	tr1 = tune("randomForest", task=ct, resampling=res, control=ctrl)
})


parallel.setup(mode="multicore", cpus=4, level="resample")
st2 = system.time({		
	tr2 = tune("randomForest", task=ct, resampling=res, control=ctrl)
})

parallel.setup(mode="multicore", cpus=4, level="tune")
st3 = system.time({		
	tr3 = tune("randomForest", task=ct, resampling=res, control=ctrl)
})


print(c(tr1$perf, tr2$perf, tr3$perf))
# [1] 0.03333333 0.03333333 0.04000000
print(c(st1[3], st2[3], st3[3]))
#elapsed elapsed elapsed
#262.068 103.702  80.458

