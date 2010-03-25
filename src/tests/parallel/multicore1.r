


library(mlr)
library(mlbench)

ct = make.classif.task(data=iris, target="Species")
wl = make.learner("randomForest.classif", ntree=30000)
res = make.res.desc("cv", iters=12)


parallel.setup(mode="local")
st1 = system.time({		
	rf1 = resample.fit(wl, ct, res)
})


parallel.setup(mode="multicore", cpus=2, level="resample")
st2 = system.time({		
	rf2 = resample.fit(wl, ct, res)
})

parallel.setup(mode="multicore", cpus=4, level="resample")
st3 = system.time({		
	rf3 = resample.fit(wl, ct, res)
})

parallel.setup(mode="multicore", cpus=6, level="resample")
st4 = system.time({		
	rf4 = resample.fit(wl, ct, res)
})

tt = p = numeric(4)

p[1] = performance(rf1)$measures["mean", "mmce"] 
p[2] = performance(rf2)$measures["mean", "mmce"] 
p[3] = performance(rf3)$measures["mean", "mmce"] 
p[4] = performance(rf4)$measures["mean", "mmce"] 

tt[1] = st1[3]
tt[2] = st2[3]
tt[3] = st3[3]
tt[4] = st4[3]

print(tt)
# [1] 40.467 21.853 13.385 11.682
print(p)
# [1] 0.04006410 0.04006410 0.04059829 0.04113248


